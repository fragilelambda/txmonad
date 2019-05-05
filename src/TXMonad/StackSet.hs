module TXMonad.StackSet
  ( StackSet(..)
  , Workspace(..)
  , Screen(..)
  , Stack(..)
  , new
  , insertUp
  , integrate'
  , modify'
  , allWindows
  , peek
  )
where

import qualified Data.List                     as L
                                                ( deleteBy
                                                , filter
                                                , find
                                                , nub
                                                , splitAt
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , listToMaybe
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

with :: b -> (Stack a -> b) -> StackSet i l a sid -> b
with dflt f = maybe dflt f . stack . workspace . current

modify
  :: Maybe (Stack a)
  -> (Stack a -> Maybe (Stack a))
  -> StackSet i l a s
  -> StackSet i l a s
modify d f s = s
  { current =
    (current s) { workspace = (workspace (current s)) { stack = with d f s } }
  }

modify' :: (Stack a -> Stack a) -> StackSet i l a s -> StackSet i l a s
modify' f = modify Nothing (Just . f)

insertUp :: Eq a => a -> StackSet i l a s -> StackSet i l a s
insertUp a s = if member a s then s else insert
 where
  insert =
    modify (Just $ Stack a [] []) (\(Stack t l r) -> Just $ Stack a l (t : r)) s

member :: Eq a => a -> StackSet i l a s -> Bool
member a s = isJust (findTag a s)

workspaces :: StackSet i l a s -> [Workspace i l a]
workspaces s = workspace (current s) : map workspace (visible s) ++ hidden s

findTag :: Eq a => a -> StackSet i l a s -> Maybe i
findTag a s = listToMaybe [ tag w | w <- workspaces s, has a (stack w) ]
 where
  has _ Nothing              = False
  has x (Just (Stack t l r)) = x `elem` (t : l ++ r)

integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l ++ x : r

integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] integrate

allWindows :: Eq a => StackSet i l a s -> [a]
allWindows = L.nub . concatMap (integrate' . stack) . workspaces

peek :: StackSet i l a s -> Maybe a
peek = with Nothing (return . focus)
