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

import           TXMonad.Core

data Full a =
  Full
  deriving (Show, Read)

instance LayoutClass Full a

data Tall a = Tall
  { tallNMaster        :: Int
  , tallRatioIncrement :: Rational
  , tallRatio          :: Rational
  } deriving (Show, Read)

instance LayoutClass Tall a

newtype Mirror l a =
  Mirror (l a)
  deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Mirror l) a

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

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a
