{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module TXMonad.Core
  (
  ) where

import           TXMonad.StackSet

data TXState = TXState
  {
  }

data TXConf = TXConf
  {
  }

data TXConfig l = TXConfig
  {
  }

type WindowSet = StackSet WorkspaceId (Layout Window) Window ScreenId

type WindowSpace = Workspace WorkspaceId (Layout Window) Window

type WorkspaceId = String

type Window = String

newtype ScreenId =
  S Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

data Layout a =
  forall l. (LayoutClass l a, Read (l a)) =>
            Layout (l a)

class Show (layout a) =>
      LayoutClass layout a
  where
  description :: layout a -> String
  description = show
