module TXMonad.Operations where

import           TXMonad.Core
import           TXMonad.Layout                 ( Full(..) )
import qualified TXMonad.StackSet              as W

import           Data.Array
import           Data.List                      ( find
                                                , intercalate
                                                , nub
                                                , (\\)
                                                )
import           Data.Maybe
import           Data.Monoid                    ( Endo(..) )
import           System.Console.ANSI
import           System.IO
import qualified Text.Read                     as T

import           Control.Monad.Reader
import           Control.Monad.State

addWindow :: TX ()
addWindow = do
  TXState { uniqueCnt = x } <- get
  modify (\s -> s { uniqueCnt = x + 1 })
  manage (show x)

deleteWindow :: TX ()
deleteWindow = withFocused unmanage

manage :: Window -> TX ()
manage w = do
  let f = W.insertUp w
  g <- appEndo <$> userCodeDef (Endo id) (return (Endo id))
  windows (g . f)

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
--
unmanage :: Window -> TX ()
unmanage = windows . W.delete

windows :: (WindowSet -> WindowSet) -> TX ()
windows = modifyWindowSet

screenString
  :: Char
  -> Char
  -> Char
  -> Char
  -> ([(Window, Rectangle)], WindowScreen)
  -> (String, [String])
screenString u d l r (rect, w) = (head, detail)
 where
  head   = screenHead w
  detail = screenDetail u d l r w rect

screenHead :: WindowScreen -> String
screenHead (W.Screen w sid sd) =
  "Screen: " ++ show (1 + fromIntegral sid :: Int) ++ " Workspace: " ++ W.tag w

screenDetail
  :: Char
  -> Char
  -> Char
  -> Char
  -> WindowScreen
  -> [(Window, Rectangle)]
  -> [String]
screenDetail u d l r (W.Screen _ _ (SD (Rectangle x y w h))) rects
  | w == 0 || h == 0
  = []
  | otherwise
  = [ [ res ! (i, j) | i <- [x .. x + w - 1] ] | j <- [y .. y + h - 1] ]
 where
  init = array
    ((x, y), (x + w - 1, y + h - 1))
    [ ((i, j), ' ') | i <- [x .. x + w - 1], j <- [y .. y + h - 1] ]
  f a t = a // windowDetail u d l r t
  res = foldl f init rects

windowDetail
  :: Char -> Char -> Char -> Char -> (Window, Rectangle) -> [((Int, Int), Char)]
windowDetail u d l r (ws, Rectangle x y w h)
  | w == 0 || h == 0 = []
  | h == 1           = up
  | h == 2           = up ++ down
  | h >= 3 && w == 1 = up ++ down ++ left
  | h >= 3 && w == 2 = up ++ down ++ left ++ right
  | otherwise        = up ++ down ++ left ++ right ++ name
 where
  up    = [ ((x + i, y), u) | i <- [0 .. w - 1] ]
  down  = [ ((x + i, y + h - 1), d) | i <- [0 .. w - 1] ]
  left  = [ ((x, y + i), l) | i <- [1 .. h - 2] ]
  right = [ ((x + w - 1, y + i), r) | i <- [1 .. h - 2] ]
  name  = zip [ (x + i, y + 1) | i <- [1 .. w - 2] ] ws

printWithColor :: Color -> IO() -> IO()
printWithColor c action = do
  setSGR [SetColor Foreground Dull c]
  action
  setSGR [Reset]

printFocusLine :: Int -> Int -> Color -> Color -> String -> IO ()
printFocusLine x w fbc nbc s = do
  printWithColor nbc $ putStr left
  printWithColor fbc $ putStr mid
  printWithColor nbc $ putStrLn right
 where
  (left, midright) = splitAt x s
  (mid , right   ) = splitAt w midright

printFocus :: Color -> Color -> Rectangle -> (String, [String]) -> IO ()
printFocus fbc nbc (Rectangle x y w h) (head, detail) = do
  printWithColor fbc $ putStrLn head
  printWithColor nbc $ putStr $ unlines up
  printMid
  printWithColor nbc $ putStr $ unlines down
 where
  (up , middown) = splitAt y detail
  (mid, down   ) = splitAt h middown
  printMid       = mapM_ (printFocusLine x w fbc nbc) mid

printDefault :: Color -> [(String, [String])] -> IO ()
printDefault nbc = mapM_ printAll
 where
  printAll (h, d) = printWithColor nbc $ putStrLn $ unlines (h : d)

printAllWithFocus
  :: [(String, [String])] -> Maybe Rectangle -> Color -> Color -> IO ()
printAllWithFocus res            Nothing  _   nbc = printDefault nbc res
printAllWithFocus (res : allRes) (Just r) fbc nbc = do
  printFocus fbc nbc r res
  printDefault nbc allRes

helpCommand :: String -> TX ()
helpCommand s = io $ do
  setCursorPosition 0 0
  clearScreen
  putStrLn s
  inputLine

inputLine :: IO ()
inputLine = do
  putStrLn "press h for help"
  putStr "txmonad> "
  hFlush stdout

printScreen :: TX ()
printScreen = do
  TXState { windowset = ws } <- get
  conf                       <- asks config
  let allScreens = W.screens ws
  rects <- forM allScreens $ \w -> do
    let wsp      = W.workspace w
        n        = W.tag wsp
        this     = W.view n ws
        tiled    = W.stack . W.workspace . W.current $ this
        viewrect = screenRect $ W.screenDetail w
    (rs, ml') <- runLayout wsp { W.stack = tiled } viewrect
    updateLayout n ml'
    return (rs, w)
  let fw    = W.peek ws
      frect = do
        fwid <- fw
        snd <$> find ((== fwid) . fst) (fst $ head rects)
      u   = upBorder conf
      d   = downBorder conf
      l   = leftBorder conf
      r   = rightBorder conf
      fbc = fromMaybe Red $ T.readMaybe (focusedBorderColor conf)
      nbc = fromMaybe Blue $ T.readMaybe (normalBorderColor conf)
  io (setCursorPosition 0 0)
  io clearScreen
  io $ printAllWithFocus (fmap (screenString u d l r) rects) frect fbc nbc
  io inputLine

updateLayout :: WorkspaceId -> Maybe (Layout Window) -> TX ()
updateLayout i ml = whenJust ml $ \l -> runOnWorkSpaces
  $ \ww -> return $ if W.tag ww == i then ww { W.layout = l } else ww

sendMessage :: Message a => a -> TX ()
sendMessage a = do
  w   <- W.workspace . W.current <$> gets windowset
  ml' <- handleMessage (W.layout w) (SomeMessage a)
  whenJust ml' $ \l' -> modifyWindowSet $ \ws -> ws
    { W.current =
      (W.current ws) { W.workspace = (W.workspace $ W.current ws)
                       { W.layout = l'
                       }
                     }
    }
  return ()

modifyWindowSet :: (WindowSet -> WindowSet) -> TX ()
modifyWindowSet f = modify $ \xst -> xst { windowset = f (windowset xst) }

-- | Return workspace visible on screen 'sc', or 'Nothing'.
screenWorkspace :: ScreenId -> TX (Maybe WorkspaceId)
screenWorkspace sc = withWindowSet $ return . W.lookupWorkspace sc

-- | Apply an 'TX' operation to the currently focused window, if there is one.
withFocused :: (Window -> TX ()) -> TX ()
withFocused f = withWindowSet $ \w -> whenJust (W.peek w) f
