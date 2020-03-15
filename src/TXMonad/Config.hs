{-# LANGUAGE TypeFamilies #-}

module TXMonad.Config
  ( Default(..)
  , Event
  )
where

import           TXMonad.Core                  as TXMonad
                                         hiding ( handleEventHook
                                                , keys
                                                , workspaces
                                                )

import           Data.Default
import qualified Data.Map                      as M
import           Data.Monoid
import qualified TXMonad.Core                  as TXMonad
                                                ( handleEventHook
                                                , keys
                                                , workspaces
                                                )
import           TXMonad.Layout
import           TXMonad.Operations
import qualified TXMonad.StackSet              as W

workspaces :: [WorkspaceId]
workspaces = map show [1 .. 3 :: Int]

handleEventHook :: Event -> TX All
handleEventHook _ = return (All True)

layout = tiled ||| Mirror tiled ||| Full
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1
  delta   = 3 / 100
  ratio   = 1 / 2

keys :: TXConfig Layout -> M.Map Event (TX ())
keys conf =
  M.fromList
    $  [ ("a" , addWindow)
       , ("x" , deleteWindow)
       , ("n" , sendMessage NextLayout)
       , ("j" , windows W.focusDown)
       , ("k" , windows W.focusUp)
       , ("sj", windows W.swapDown)
       , ("sk", windows W.swapUp)
       , ("," , sendMessage (IncMasterN 1))
       , ("." , sendMessage (IncMasterN (-1)))
       , ("h" , sendMessage Shrink)
       , ("l" , sendMessage Expand)
       , ("m" , windows W.focusMaster)
       , ("sm", windows W.swapMaster)
       ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
       [ (m ++ show k, windows $ f i)
       | (i, k) <- zip (TXMonad.workspaces conf) [1 .. 9]
       , (f, m) <- [(W.greedyView, "j"), (W.shift, "sj")]
       ]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
       [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip ["w", "e", "r"] [0 ..]
       , (f, m) <- [(W.view, "j"), (W.shift, "sj")]
       ]

instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) =>
         Default (TXConfig a) where
  def = TXConfig { TXMonad.workspaces      = workspaces
                 , TXMonad.layoutHook      = layout
                 , TXMonad.keys            = keys
                 , TXMonad.handleEventHook = handleEventHook
                 , TXMonad.sd              = [SD (Rectangle 0 0 160 90), SD (Rectangle 0 0 40 30)]
                 }
