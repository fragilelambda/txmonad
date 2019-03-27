{-# LANGUAGE TypeFamilies #-}
module TXMonad.Config
  ( Default(..)
  , Event
  )
where

import           TXMonad.Core                  as TXMonad
                                         hiding ( workspaces
                                                , handleEventHook
                                                )

import qualified TXMonad.Core                  as TXMonad
                                                ( workspaces
                                                , handleEventHook
                                                )
import           TXMonad.Layout
import           Data.Default
import           Data.Monoid

workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

handleEventHook :: Event -> TX All
handleEventHook _ = return (All True)

layout = tiled ||| Mirror tiled ||| Full
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1
  delta   = 1 / 2
  ratio   = 3 / 100

instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) =>
         Default (TXConfig a) where
  def = TXConfig { TXMonad.workspaces      = workspaces
                 , TXMonad.layoutHook      = layout
                 , TXMonad.handleEventHook = handleEventHook
                 , TXMonad.sd              = 10
                 }
