{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TXMonad.Layout
  ( Full(..)
  , Tall(..)
  , Mirror(..)
  , Choose
  , (|||)
  )
where

import           Control.Arrow                  ( second
                                                , (***)
                                                )
import           Control.Monad
import           Data.Maybe                     ( fromMaybe )
import           TXMonad.Core
import           TXMonad.StackSet              as W

data Resize
  = Shrink
  | Expand
  deriving (Typeable)

data IncMasterN =
  IncMasterN Int
  deriving (Typeable)

instance Message Resize

instance Message IncMasterN

data Full a =
  Full
  deriving (Show, Read)

instance LayoutClass Full a

data Tall a = Tall
  { tallNMaster        :: Int
  , tallRatioIncrement :: Rational
  , tallRatio          :: Rational
  } deriving (Show, Read)

instance LayoutClass Tall a where
  pureLayout (Tall nmaster _ frac) r s = zip ws rs
   where
    ws = W.integrate s
    rs = tile frac r nmaster (length ws)
  pureMessage (Tall nmaster delta frac) m = msum
    [fmap resize (fromMessage m), fmap incmastern (fromMessage m)]
   where
    resize Shrink = Tall nmaster delta (max 0 $ frac - delta)
    resize Expand = Tall nmaster delta (min 1 $ frac + delta)
    incmastern (IncMasterN d) = Tall (max 0 (nmaster + d)) delta frac

tile :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile f r nmaster n = if n <= nmaster || nmaster == 0
  then splitVertically n r
  else splitVertically nmaster r1 ++ splitVertically (n - nmaster) r2
  where (r1, r2) = splitHorizontallyBy f r

splitVertically, splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) =
  Rectangle sx sy sw smallh
    : splitVertically (n - 1) (Rectangle sx (sy + smallh) sw (sh - smallh))
  where smallh = sh `div` n

splitHorizontally n = map mirrorRect . splitVertically n . mirrorRect

splitHorizontallyBy, splitVerticallyBy
  :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
  (Rectangle sx sy leftw sh, Rectangle (sx + leftw) sy (sw - leftw) sh)
  where leftw = floor $ fromIntegral sw * f

splitVerticallyBy f =
  (mirrorRect *** mirrorRect) . splitHorizontallyBy f . mirrorRect

mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = Rectangle ry rx rh rw

newtype Mirror l a =
  Mirror (l a)
  deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Mirror l) a where
  runLayout (W.Workspace i (Mirror l) ms) r =
    (map (second mirrorRect) *** fmap Mirror)
      `fmap` runLayout (W.Workspace i l ms) (mirrorRect r)
  handleMessage (Mirror l) = fmap (fmap Mirror) . handleMessage l
  description (Mirror l) = "Mirror " ++ description l

(|||) :: l a -> r a -> Choose l r a
(|||) = Choose L

infixr 5 |||

data Choose l r a =
  Choose LR
         (l a)
         (r a)
  deriving (Read, Show)

data LR
  = L
  | R
  deriving (Read, Show, Eq)

data ChangeLayout
  = FirstLayout
  | NextLayout
  deriving (Eq, Show, Typeable)

instance Message ChangeLayout

data NextNoWrap =
  NextNoWrap
  deriving (Eq, Show, Typeable)

instance Message NextNoWrap

handle :: (LayoutClass l a, Message m) => l a -> m -> TX (Maybe (l a))
handle l m = handleMessage l (SomeMessage m)

choose
  :: (LayoutClass l a, LayoutClass r a)
  => Choose l r a
  -> LR
  -> Maybe (l a)
  -> Maybe (r a)
  -> TX (Maybe (Choose l r a))
choose (Choose d _ _) d' Nothing Nothing | d == d' = return Nothing
choose (Choose d l r) d' ml mr                     = f lr
 where
  (l', r') = (fromMaybe l ml, fromMaybe r mr)
  lr       = case (d, d') of
    (L, R) -> (hide l', return r')
    (R, L) -> (return l', hide r')
    (_, _) -> (return l', return r')
  f (x, y) = Just <$> liftM2 (Choose d') x y
  hide x = fromMaybe x <$> handle x Hide

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
  runLayout (W.Workspace i (Choose L l r) ms) =
    fmap (second . fmap $ flip (Choose L) r) . runLayout (W.Workspace i l ms)
  runLayout (W.Workspace i (Choose R l r) ms) =
    fmap (second . fmap $ Choose R l) . runLayout (W.Workspace i r ms)
  description (Choose L l _) = description l
  description (Choose R _ r) = description r
  handleMessage lr m | Just NextLayout <- fromMessage m = do
    mlr' <- handle lr NextNoWrap
    maybe (handle lr FirstLayout) (return . Just) mlr'
  handleMessage c@(Choose d l r) m | Just NextNoWrap <- fromMessage m =
    case d of
      L -> do
        ml <- handle l NextNoWrap
        case ml of
          Just _  -> choose c L ml Nothing
          Nothing -> choose c R Nothing =<< handle r FirstLayout
      R -> choose c R Nothing =<< handle r NextNoWrap
  handleMessage c@(Choose _ l _) m | Just FirstLayout <- fromMessage m =
    flip (choose c L) Nothing =<< handle l FirstLayout
  handleMessage c@(Choose d l r) m | Just ReleaseResources <- fromMessage m =
    join $ liftM2 (choose c d)
                  (handle l ReleaseResources)
                  (handle r ReleaseResources)
  handleMessage c@(Choose d l r) m = do
    ml' <- case d of
      L -> handleMessage l m
      R -> return Nothing
    mr' <- case d of
      L -> return Nothing
      R -> handleMessage r m
    choose c d ml' mr'
