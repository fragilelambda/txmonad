module TXMonad.Operations where

import           TXMonad.Core
import qualified TXMonad.StackSet              as W

import           Data.Monoid                    ( Endo(..) )
import           Data.List                      ( nub
                                                , (\\)
                                                , find
                                                )
import           Data.Matrix

import           Control.Monad.State
import           Control.Monad.Reader


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
  return ()

setWindowBorder :: Window -> Char -> Window
setWindowBorder w nbc =
  mapRow f 0 . mapRow f row . mapCol f 0 . mapCol f col $ w
 where
  row = nrows w - 1
  col = ncols w - 1
  f _ _ = nbc
