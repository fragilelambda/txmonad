{-# LANGUAGE TypeFamilies #-}
module TXMonad.Config
  ( Default(..)
  )
where

import           TXMonad.Core                  as TXMonad
                                         hiding ( workspaces )

import qualified TXMonad.Core                  as TXMonad
                                                ( workspaces )
import           TXMonad.Layout
import           Data.Default

workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

layout = tiled ||| Mirror tiled ||| Full
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1
  delta   = 1 / 2
  ratio   = 3 / 100

instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) =>
         Default (TXConfig a) where
  def = TXConfig { TXMonad.workspaces = workspaces
                 , TXMonad.layoutHook = layout
                 , sd                 = 10
                 }
