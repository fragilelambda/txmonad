module TXMonad.StackSet
  ( StackSet(..)
  , Workspace(..)
  , Screen(..)
  , Stack(..)
  , new
  )
where

import qualified Data.List                     as L
                                                ( deleteBy
                                                , filter
                                                , find
                                                , nub
                                                , splitAt
                                                )

data StackSet i l a sid = StackSet
  { current :: Screen i l a sid
  , visible :: [Screen i l a sid]
  , hidden  :: [Workspace i l a]
  } deriving (Show, Read, Eq)

data Screen i l a sid = Screen
  { workspace :: Workspace i l a
  , screen    :: sid
  } deriving (Show, Read, Eq)

data Workspace i l a = Workspace
  { tag    :: i
  , layout :: l
  , stack  :: Maybe (Stack a)
  } deriving (Show, Read, Eq)

data Stack a = Stack
  { focus :: a
  , up    :: [a]
  , down  :: [a]
  } deriving (Show, Read, Eq)

new :: (Integral s) => l -> [i] -> Int -> StackSet i l a s
new l wids m = StackSet cur visi unseen
 where
  (seen, unseen) = L.splitAt m $ map (\i -> Workspace i l Nothing) wids
  (cur : visi)   = [ Screen i s | (i, s) <- zip seen [0 ..] ]
