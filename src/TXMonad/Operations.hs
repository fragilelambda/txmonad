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
import           Data.Monoid                    ( Endo(..) )

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

screenString :: ([(Window, Rectangle)], WindowScreen) -> String
screenString (rect, w) = unlines $ head : detail
 where
  head   = screenHead "" w
  detail = screenDetail "" w rect

screenHead :: String -> WindowScreen -> String
screenHead color (W.Screen w sid sd) =
  "Screen: " ++ show (fromIntegral sid :: Int) ++ " Workspace: " ++ W.tag w

screenDetail :: String -> WindowScreen -> [(Window, Rectangle)] -> [String]
screenDetail color (W.Screen _ _ (SD (Rectangle x y w h))) rects
  | w == 0 || h == 0
  = []
  | otherwise
  = [ [ res ! (i, j) | i <- [x .. x + w - 1] ] | j <- [y .. y + h - 1] ]
 where
  init = array
    ((x, y), (x + w - 1, y + h - 1))
    [ ((i, j), ' ') | i <- [x .. x + w - 1], j <- [y .. y + h - 1] ]
  f a t = a // windowDetail t
  res = foldl f init rects

windowDetail :: (Window, Rectangle) -> [((Int, Int), Char)]
windowDetail (ws, Rectangle x y w h)
  | w == 0 || h == 0 = []
  | h == 1           = up
  | h == 2           = up ++ down
  | h >= 3 && w == 1 = up ++ down ++ left
  | h >= 3 && w == 2 = up ++ down ++ left ++ right
  | otherwise        = up ++ down ++ left ++ right ++ name
 where
  up    = [ ((x + i, y), '▄') | i <- [0 .. w - 1] ]
  down  = [ ((x + i, y + h - 1), '▀') | i <- [0 .. w - 1] ]
  left  = [ ((x, y + i), '▌') | i <- [1 .. h - 2] ]
  right = [ ((x + w - 1, y + i), '▐') | i <- [1 .. h - 2] ]
  name  = zip [ (x + 1, y + i) | i <- [1 .. w - 2] ] ws

printScreen :: TX ()
printScreen = do
  TXState { windowset = ws } <- get
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
  io $ putStrLn $ intercalate "\n" $ fmap screenString rects

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
