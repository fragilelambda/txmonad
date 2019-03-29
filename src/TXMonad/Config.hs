{-# LANGUAGE TypeFamilies #-}
module TXMonad.Config
  ( Default(..)
  , Event
  )
where

import           TXMonad.Core                  as TXMonad
                                         hiding ( workspaces
                                                , handleEventHook
                                                , keys
                                                )

import qualified TXMonad.Core                  as TXMonad
                                                ( workspaces
                                                , handleEventHook
                                                , keys
                                                )
import           TXMonad.Layout
import           Data.Default
import           Data.Monoid
import qualified Data.Map                      as M

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
