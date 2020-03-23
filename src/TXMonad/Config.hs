{-# LANGUAGE TypeFamilies #-}

module TXMonad.Config
  ( Default(..)
  , Event
  )
where

import           TXMonad.Core                  as TXMonad
                                         hiding ( handleEventHook
                                                , keys
                                                , screenEventHook
                                                , workspaces
                                                )

import           Data.Default
import qualified Data.Map                      as M
import           Data.Monoid
import           System.Exit
import qualified TXMonad.Core                  as TXMonad
                                                ( handleEventHook
                                                , keys
                                                , screenEventHook
                                                , workspaces
                                                )
import           TXMonad.Layout
import           TXMonad.Operations
import qualified TXMonad.StackSet              as W

workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

handleEventHook :: Event -> TX All
handleEventHook _ = return (All True)

screenEventHook :: Event -> TX All
screenEventHook "h" = return (All False)
screenEventHook _   = return (All True)

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
       , ("h" , helpCommand help)
       , ("q" , io exitSuccess)
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
  def = TXConfig
    { TXMonad.workspaces         = workspaces
    , TXMonad.layoutHook         = layout
    , TXMonad.keys               = keys
    , TXMonad.handleEventHook    = handleEventHook
    , TXMonad.screenEventHook    = screenEventHook
    , TXMonad.sd = [SD (Rectangle 0 0 80 20), SD (Rectangle 0 0 40 20)]
    , TXMonad.normalBorderColor  = "Blue"
    , TXMonad.focusedBorderColor = "Red"
    , TXMonad.upBorder           = '▄'
    , TXMonad.downBorder         = '▀'
    , TXMonad.leftBorder         = '▌'
    , TXMonad.rightBorder        = '▐'
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines
  [ "-- launching and killing programs"
  , "a        Add one focused window"
  , "x        Close the focused window"
  , "n        Rotate through the available layout algorithms"
  , ""
  , "-- move focus up or down the window stack"
  , "j        Move focus to the next window"
  , "k        Move focus to the previous window"
  , "m        Move focus to the master window"
  , ""
  , "-- modifying the window order"
  , "sj       Swap the focused window with the next window"
  , "sk       Swap the focused window with the previous window"
  , "sm       Swap the focused window and the master window"
  , ""
  , "-- resizing the master/slave ratio"
  , "h        Shrink the master area"
  , "l        Expand the master area"
  , ""
  , "-- increase or decrease number of windows in the master area"
  , "comma  (,)      Increment the number of windows in the master area"
  , "period (.)      Deincrement the number of windows in the master area"
  , ""
  , "-- quit, or restart"
  , "q               Quit txmonad"
  , ""
  , "-- Workspaces & screens"
  , "j[1..9]         Switch to workSpace N"
  , "sj[1..9]        Move client to workspace N"
  , "j{w,e,r}        Switch to screen 1, 2, or 3"
  , "sj{w,e,r}       Move client to screen 1, 2, or 3"
  ]
