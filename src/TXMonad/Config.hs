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

keys :: TXConfig Layout -> M.Map Event (TX ())
keys conf = M.fromList $ []

instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) =>
         Default (TXConfig a) where
  def = TXConfig { TXMonad.workspaces      = workspaces
                 , TXMonad.layoutHook      = layout
                 , TXMonad.keys            = keys
                 , TXMonad.handleEventHook = handleEventHook
                 , TXMonad.sd              = 10
                 }
