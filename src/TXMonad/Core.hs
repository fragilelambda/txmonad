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
  , Event
  , ScreenId(..)
  , TXState(..)
  , TXConf(..)
  , TXConfig(..)
  , LayoutClass(..)
  , Layout(..)
  , runTX
  , catchTX
  , userCode
  , userCodeDef
  , whenTX
  , whenJust
  , io
  )
where

import           TXMonad.StackSet

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Default
import qualified Data.Map                      as M
import           Data.Matrix
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Monoid

data TXState = TXState
  { windowset :: WindowSet
  }

data TXConf = TXConf
  { config        :: TXConfig Layout
  , normalBorder  :: Char
  , focusedBorder :: Char
  , keyActions    :: M.Map Event (TX ())
  }

data TXConfig l = TXConfig
  { layoutHook      :: l Window
  , workspaces      :: [String]
  , keys            :: TXConfig Layout -> M.Map Event (TX ())
  , sd              :: Int
  , handleEventHook :: Event -> TX All
  }

data Rectangle = Rectangle
  { x      :: Int
  , y      :: Int
  , width  :: Int
  , height :: Int
  }

type WindowSet = StackSet WorkspaceId (Layout Window) Window ScreenId

type WindowSpace = Workspace WorkspaceId (Layout Window) Window

type WorkspaceId = String

type Window = Matrix Char

type Event = String

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

catchTX :: TX a -> TX a -> TX a
catchTX job errcase = do
  st      <- get
  c       <- ask
  (a, s') <- io $ runTX c st job
  put s'
  return a

userCode :: TX a -> TX (Maybe a)
userCode a = catchTX (Just `liftM` a) (return Nothing)

userCodeDef :: a -> TX a -> TX a
userCodeDef defValue a = fromMaybe defValue `liftM` userCode a

data Layout a =
  forall l. (LayoutClass l a, Read (l a)) =>
            Layout (l a)

class Show (layout a) =>
      LayoutClass layout a
  where
  runLayout :: Workspace WorkspaceId (layout a) a -> Rectangle
  doLayout ::
       layout a
    -> Rectangle
    -> Stack a
    -> TX ([(a, Rectangle)], Maybe (layout a))
  pureLayout :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
  description :: layout a -> String
  description = show

whenTX :: TX Bool -> TX () -> TX ()
whenTX a f = a >>= \b -> when b f

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

io :: MonadIO m => IO a -> m a
io = liftIO
