{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module TXMonad.Core
  ( TX
  , WindowSet
  , WindowSpace
  , WorkspaceId
  , Window
  , ScreenId(..)
  , TXState(..)
  , TXConf(..)
  , TXConfig(..)
  , LayoutClass(..)
  , Layout(..)
  , runTX
  , io
  )
where

import           TXMonad.StackSet

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Default

data TXState = TXState
  { windowset :: WindowSet
  }

data TXConf = TXConf
  { config :: TXConfig Layout
  }

data TXConfig l = TXConfig
  { layoutHook :: (l Window)
  , workspaces :: [String]
  , sd         :: Int
  }

type WindowSet = StackSet WorkspaceId (Layout Window) Window ScreenId

type WindowSpace = Workspace WorkspaceId (Layout Window) Window

type WorkspaceId = String

type Window = String

newtype ScreenId =
  S Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

newtype TX a =
  TX (ReaderT TXConf (StateT TXState IO) a)
  deriving (Functor, Monad, MonadIO, MonadState TXState, MonadReader TXConf)

instance Applicative TX where
  pure  = return
  (<*>) = ap

instance Semigroup a => Semigroup (TX a) where
  (<>) = liftM2 (<>)

instance (Monoid a) => Monoid (TX a) where
  mempty  = return mempty
  mappend = liftM2 mappend

instance Default a => Default (TX a) where
  def = return def

runTX :: TXConf -> TXState -> TX a -> IO (a, TXState)
runTX c st (TX a) = runStateT (runReaderT a c) st

data Layout a =
  forall l. (LayoutClass l a, Read (l a)) =>
            Layout (l a)

class Show (layout a) =>
      LayoutClass layout a
  where
  description :: layout a -> String
  description = show

io :: MonadIO m => IO a -> m a
io = liftIO
