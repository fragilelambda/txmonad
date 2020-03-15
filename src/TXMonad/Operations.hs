module TXMonad.Operations where

import           TXMonad.Core
import           TXMonad.Layout                 ( Full(..) )
import qualified TXMonad.StackSet              as W

import           Data.List                      ( find
                                                , nub
                                                , (\\)
                                                )
import           Data.Matrix
import           Data.Monoid                    ( Endo(..) )

import           Control.Monad.Reader
import           Control.Monad.State

addWindow :: TX ()
addWindow = do
  TXState { uniqueCnt = x } <- get
  modify (\s -> s { uniqueCnt = x + 1 })
  manage (show x)

deleteWindow :: TX()
deleteWindow = withFocused unmanage

manage :: Window -> TX ()
manage w = do
  let f ws = W.insertUp w ws
  g <- appEndo <$> userCodeDef (Endo id) (return (Endo id))
  windows (g . f)

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
--
unmanage :: Window -> TX ()
unmanage = windows . W.delete

windows :: (WindowSet -> WindowSet) -> TX ()
windows = modifyWindowSet

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
    return rs
  io $ print $ show rects

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
