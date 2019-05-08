{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

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
  , Typeable
  , Message
  , Rectangle(..)
  , SomeMessage(..)
  , LayoutMessages(..)
  , ScreenDetail(..)
  , fromMessage
  , runTX
  , catchTX
  , runOnWorkSpaces
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
import           Data.Typeable

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
  , sd              :: [ScreenDetail]
  , handleEventHook :: Event -> TX All
  }

data Rectangle = Rectangle
  { x      :: Int
  , y      :: Int
  , width  :: Int
  , height :: Int
  } deriving (Eq, Show, Read)

type WindowSet
  = StackSet WorkspaceId (Layout Window) Window ScreenId ScreenDetail

type WindowSpace = Workspace WorkspaceId (Layout Window) Window

type WorkspaceId = String

type Window = Matrix Char

type Event = String

newtype ScreenId =
  S Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

data ScreenDetail = SD
  { screenRect :: Rectangle
  } deriving (Eq, Show, Read)

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
  runLayout ::
       Workspace WorkspaceId (layout a) a
    -> Rectangle
    -> TX ([(a, Rectangle)], Maybe (layout a))
  runLayout (Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms
  doLayout ::
       layout a
    -> Rectangle
    -> Stack a
    -> TX ([(a, Rectangle)], Maybe (layout a))
  doLayout l r s = return (pureLayout l r s, Nothing)
  pureLayout :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
  pureLayout _ r s = [(focus s, r)]
  emptyLayout ::
       layout a -> Rectangle -> TX ([(a, Rectangle)], Maybe (layout a))
  emptyLayout _ _ = return ([], Nothing)
  handleMessage :: layout a -> SomeMessage -> TX (Maybe (layout a))
  handleMessage l = return . pureMessage l
  pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
  pureMessage _ _ = Nothing
  description :: layout a -> String
  description = show

instance Show (Layout a) where
  show (Layout l) = show l

instance LayoutClass Layout Window where
  runLayout (Workspace i (Layout l) ms) r =
    fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
  doLayout (Layout l) r s = fmap (fmap Layout) `fmap` doLayout l r s
  emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
  handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
  description (Layout l) = description l

class Typeable a =>
      Message a


data SomeMessage =
  forall a. Message a =>
            SomeMessage a

fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

instance Message Event

data LayoutMessages
  = Hide
  | ReleaseResources
  deriving (Typeable, Eq)

instance Message LayoutMessages

whenTX :: TX Bool -> TX () -> TX ()
whenTX a f = a >>= \b -> when b f

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

io :: MonadIO m => IO a -> m a
io = liftIO

runOnWorkSpaces :: (WindowSpace -> TX WindowSpace) -> TX ()
runOnWorkSpaces job = do
  ws    <- gets windowset
  h     <- mapM job $ hidden ws
  c : v <-
    mapM (\s -> (\w -> s { workspace = w }) <$> job (workspace s))
    $ current ws
    : visible ws
  modify $ \s -> s { windowset = ws { current = c, visible = v, hidden = h } }
