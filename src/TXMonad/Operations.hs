module TXMonad.Operations where

import           TXMonad.Core
import qualified TXMonad.StackSet              as W

import           Data.List                      ( find
                                                , nub
                                                , (\\)
                                                )
import           Data.Matrix
import           Data.Monoid                    ( Endo(..) )

import           Control.Monad.Reader
import           Control.Monad.State

manage :: Window -> TX ()
manage w = do
  let f ws = W.insertUp w ws
  g <- appEndo <$> userCodeDef (Endo id) (return (Endo id))
  windows (g . f)

windows :: (WindowSet -> WindowSet) -> TX ()
windows f = do
  TXState { windowset = old }                        <- get
  TXConf { normalBorder = nbc, focusedBorder = fbc } <- ask
  whenJust (W.peek old) $ \otherw -> do
    let ws =
          W.modify' (\s -> s { W.focus = setWindowBorder (W.focus s) nbc }) old
    modify (\s -> s { windowset = ws })
  TXState { windowset = old } <- get
  let oldVisible =
        concatMap (W.integrate' . W.stack . W.workspace)
          $ W.current old
          : W.visible old
      ws         = f old
      newwindows = W.allWindows ws \\ W.allWindows old
  modify (\s -> s { windowset = ws })
  let tags_oldvisible =
        map (W.tag . W.workspace) $ W.current old : W.visible old
      gottenhidden = filter (flip elem tags_oldvisible . W.tag) $ W.hidden ws
  mapM_ (sendMessageWithNoRefresh Hide) gottenhidden
  let allscreens = W.screens ws
      summed_visible = scanl (++) [] $ map (W.integrate' . W.stack . W.workspace) allscreens
  -- rects <- fmap concat $ forM (zip allscreens summed_visible) $ \ (w, vis) -> do
  --   let wsp = W.workspace w
  --       tiled = W.stack wsp
  --       viewrect = w
  return ()

setWindowBorder :: Window -> Char -> Window
setWindowBorder w nbc =
  mapRow f 0 . mapRow f row . mapCol f 0 . mapCol f col $ w
 where
  row = nrows w - 1
  col = ncols w - 1
  f _ _ = nbc

sendMessageWithNoRefresh :: Message a => a -> WindowSpace -> TX ()
sendMessageWithNoRefresh a w =
  handleMessage (W.layout w) (SomeMessage a)
    `catchTX` return Nothing
    >>=       updateLayout (W.tag w)

updateLayout :: WorkspaceId -> Maybe (Layout Window) -> TX ()
updateLayout i ml = whenJust ml $ \l -> runOnWorkSpaces
  $ \ww -> return $ if W.tag ww == i then ww { W.layout = l } else ww
