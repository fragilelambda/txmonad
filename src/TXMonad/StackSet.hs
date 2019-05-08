module TXMonad.StackSet
  ( StackSet(..)
  , Workspace(..)
  , Screen(..)
  , Stack(..)
  , new
  , insertUp
  , integrate
  , integrate'
  , modify'
  , allWindows
  , peek
  , screens
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

data StackSet i l a sid sd = StackSet
  { current :: Screen i l a sid sd
  , visible :: [Screen i l a sid sd]
  , hidden  :: [Workspace i l a]
  } deriving (Show, Read, Eq)

data Screen i l a sid  sd = Screen
  { workspace :: Workspace i l a
  , screen    :: sid
  , screenDetail :: sd
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

new :: (Integral s) => l -> [i] -> [sd] -> StackSet i l a s sd
new l wids m = StackSet cur visi unseen
 where
  (seen, unseen) = L.splitAt (length m) $ map (\i -> Workspace i l Nothing) wids
  (cur : visi)   = [ Screen i s sd | (i, s, sd) <- zip3 seen [0 ..] m ]

with :: b -> (Stack a -> b) -> StackSet i l a sid sd -> b
with dflt f = maybe dflt f . stack . workspace . current

modify
  :: Maybe (Stack a)
  -> (Stack a -> Maybe (Stack a))
  -> StackSet i l a s sd
  -> StackSet i l a s sd
modify d f s = s
  { current =
    (current s) { workspace = (workspace (current s)) { stack = with d f s } }
  }

modify' :: (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify' f = modify Nothing (Just . f)

insertUp :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
insertUp a s = if member a s then s else insert
 where
  insert =
    modify (Just $ Stack a [] []) (\(Stack t l r) -> Just $ Stack a l (t : r)) s

member :: Eq a => a -> StackSet i l a s sd -> Bool
member a s = isJust (findTag a s)

workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces s = workspace (current s) : map workspace (visible s) ++ hidden s

findTag :: Eq a => a -> StackSet i l a s sd -> Maybe i
findTag a s = listToMaybe [ tag w | w <- workspaces s, has a (stack w) ]
 where
  has _ Nothing              = False
  has x (Just (Stack t l r)) = x `elem` (t : l ++ r)

integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l ++ x : r

integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] integrate

allWindows :: Eq a => StackSet i l a s sd -> [a]
allWindows = L.nub . concatMap (integrate' . stack) . workspaces

peek :: StackSet i l a s sd -> Maybe a
peek = with Nothing (return . focus)

screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens s = current s : visible s
