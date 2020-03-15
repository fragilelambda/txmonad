module TXMonad.StackSet
  ( StackSet(..)
  , Workspace(..)
  , Screen(..)
  , Stack(..)
  , new
  , view
  , insertUp
  , integrate
  , integrate'
  , modify'
  , allWindows
  , peek
  , screens
  , swapUp
  , swapDown
  , focusUp
  , focusDown
  , focusMaster
  , swapMaster
  , shift
  , greedyView
  , lookupWorkspace
  , delete
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
import           Prelude                 hiding ( filter )

data StackSet i l a sid sd = StackSet
  { current :: Screen i l a sid sd
  , visible :: [Screen i l a sid sd]
  , hidden  :: [Workspace i l a]
  } deriving (Show, Read, Eq)

data Screen i l a sid sd = Screen
  { workspace    :: Workspace i l a
  , screen       :: sid
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
  (seen, unseen) =
    L.splitAt (length m) $ map (\i -> Workspace i l Nothing) wids
  (cur : visi) = [ Screen i s sd | (i, s, sd) <- zip3 seen [0 ..] m ]

view :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
view i s
  | i == currentTag s = s
  |
    -- current
    Just x <- L.find ((i ==) . tag . workspace) (visible s)
    -- if it is visible, it is just raised
                                                            = s
    { current = x
    , visible = current s : L.deleteBy (equating screen) x (visible s)
    }
  | Just x <- L.find ((i ==) . tag) (hidden s) -- must be hidden then
    -- if it was hidden, it is raised on the xine screen currently used
                                               = s
    { current = (current s) { workspace = x }
    , hidden  = workspace (current s) : L.deleteBy (equating tag) x (hidden s)
    }
  | otherwise = s -- not a member of the stackset
  where equating f = \x y -> f x == f y

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

currentTag :: StackSet i l a s sd -> i
currentTag = tag . workspace . current

peek :: StackSet i l a s sd -> Maybe a
peek = with Nothing (return . focus)

screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens s = current s : visible s

focusUp, focusDown, swapUp, swapDown
  :: StackSet i l a s sd -> StackSet i l a s sd
focusUp = modify' focusUp'

focusDown = modify' focusDown'

swapUp = modify' swapUp'

swapDown = modify' (reverseStack . swapUp' . reverseStack)

focusUp', focusDown' :: Stack a -> Stack a
focusUp' (Stack t (l : ls) rs) = Stack l ls (t : rs)
focusUp' (Stack t []       rs) = Stack x xs [] where (x : xs) = reverse (t : rs)

focusDown' = reverseStack . focusUp' . reverseStack

swapUp' :: Stack a -> Stack a
swapUp' (Stack t (l : ls) rs) = Stack t ls (l : rs)
swapUp' (Stack t []       rs) = Stack t (reverse rs) []

reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls

swapMaster :: StackSet i l a s sd -> StackSet i l a s sd
swapMaster = modify' $ \c -> case c of
  Stack _ [] _  -> c -- already master.
  Stack t ls rs -> Stack t [] (xs ++ x : rs) where (x : xs) = reverse ls

focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster = modify' $ \c -> case c of
  Stack _ [] _  -> c
  Stack t ls rs -> Stack x [] (xs ++ t : rs) where (x : xs) = reverse ls

--
-- ---------------------------------------------------------------------
-- $composite
-- | /O(w)/. shift. Move the focused element of the current stack to stack
-- 'n', leaving it as the focused element on that stack. The item is
-- inserted above the currently focused element on that workspace.
-- The actual focused workspace doesn't change. If there is no
-- element on the current stack, the original stackSet is returned.
--
shift :: (Ord a, Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift n s = maybe s (\w -> shiftWin n w s) (peek s)

-- | /O(n)/. shiftWin. Searches for the specified window 'w' on all workspaces
-- of the stackSet and moves it to stack 'n', leaving it as the focused
-- element on that stack. The item is inserted above the currently
-- focused element on that workspace.
-- The actual focused workspace doesn't change. If the window is not
-- found in the stackSet, the original stackSet is returned.
shiftWin
  :: (Ord a, Eq s, Eq i) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin n w s = case findTag w s of
  Just from | n `tagMember` s && n /= from -> go from s
  _ -> s
  where go from = onWorkspace n (insertUp w) . onWorkspace from (delete' w)

onWorkspace
  :: (Eq i, Eq s)
  => i
  -> (StackSet i l a s sd -> StackSet i l a s sd)
  -> (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace n f s = view (currentTag s) . f . view n $ s

-- |
-- /O(1) on current window, O(n) in general/. Delete window 'w' if it exists.
-- There are 4 cases to consider:
--
--   * delete on an 'Nothing' workspace leaves it Nothing
--
--   * otherwise, try to move focus to the down
--
--   * otherwise, try to move focus to the up
--
--   * otherwise, you've got an empty workspace, becomes 'Nothing'
--
-- Behaviour with respect to the master:
--
--   * deleting the master window resets it to the newly focused window
--
--   * otherwise, delete doesn't affect the master.
--
delete :: (Ord a) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete = delete'

-- | Only temporarily remove the window from the stack, thereby not destroying special
-- information saved in the 'Stackset'
delete' :: (Eq a) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete' w s = s { current = removeFromScreen (current s)
                , visible = map removeFromScreen (visible s)
                , hidden  = map removeFromWorkspace (hidden s)
                }
 where
  removeFromWorkspace ws = ws { stack = stack ws >>= filter (/= w) }
  removeFromScreen scr =
    scr { workspace = removeFromWorkspace (workspace scr) }

-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- 'True'.  Order is preserved, and focus moves as described for 'delete'.
--
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p (Stack f ls rs) = case L.filter p (f : rs) of
  f' : rs' -> Just $ Stack f' (L.filter p ls) rs' -- maybe move focus down
  []       -> case L.filter p ls of
    f' : ls' -> Just $ Stack f' ls' [] -- else up
    []       -> Nothing -- filter back up

-- | Is the given tag present in the 'StackSet'?
tagMember :: Eq i => i -> StackSet i l a s sd -> Bool
tagMember t = elem t . map tag . workspaces

-- |
-- Set focus to the given workspace.  If that workspace does not exist
-- in the stackset, the original workspace is returned.  If that workspace is
-- 'hidden', then display that workspace on the current screen, and move the
-- current workspace to 'hidden'.  If that workspace is 'visible' on another
-- screen, the workspaces of the current screen and the other screen are
-- swapped.
greedyView :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
greedyView w ws
  | any wTag (hidden ws) = view w ws
  | (Just s) <- L.find (wTag . workspace) (visible ws) = ws
    { current = (current ws) { workspace = workspace s }
    , visible = s { workspace = workspace (current ws) }
                  : L.filter (not . wTag . workspace) (visible ws)
    }
  | otherwise = ws
  where wTag = (w ==) . tag

-- | Find the tag of the workspace visible on Xinerama screen 'sc'.
-- 'Nothing' if screen is out of bounds.
lookupWorkspace :: Eq s => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace sc w = listToMaybe [ tag i | Screen i s _ <- current w : visible w, s == sc ]
