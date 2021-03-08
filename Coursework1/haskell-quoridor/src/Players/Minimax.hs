{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}
module Players.Minimax where

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List
import Data.Array

import Types
import Constants
import Cell
import Action
import Board
import Player
import Game
import Players.Dumb (dumbAction)

{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + maximum (map (stateTreeDepth . snd) ts)

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as

{- 
    *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]
generateGameTree :: Game -> GameTree
generateGameTree (Game b ps) = StateTree (Game b ps) [ ( a , generateGameTree (fromJust (performAction (Game b ps) a) ) )  | a <- validActions (Game b ps), generateGameTreeSpeedUp a ps]

generateGameTreeHelp :: Maybe Game -> Game
generateGameTreeHelp (Just g) = g
generateGameTreeHelp Nothing = undefined

generateGameTreeSpeedUp :: Action -> [Player]-> Bool
-- generateGameTreeSpeedUp _ _ = True
generateGameTreeSpeedUp (Place ((c1, c2),(c3, c4))) ps = or[(c1 == currentCell p) || (c2 == currentCell p) || (c3 == currentCell p) || (c4 == currentCell p) | p <- ps]
generateGameTreeSpeedUp (Move _) p = True

{-
    *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}

-- Higher scoring nodes go first.
-- [Hint: You should use 'lowFirst'.]
highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree v a) = StateTree v (sortBy highFirstHelper [(a1, lowFirst v1)| (a1,v1)<-a] )

highFirstHelper (_, StateTree v1 _) (_, StateTree v2 _)
  | v1 < v2 = GT
  | v1 > v2 = LT
  | v1 == v2 = EQ

-- Lower scoring nodes go first.
-- [Hint: You should use 'highFirst'.]
lowFirst :: (Ord v) => StateTree v a -> StateTree v a
lowFirst (StateTree v a)  = StateTree v (sortBy lowFirstHelper [(a1, highFirst v1)| (a1,v1)<-a] )

lowFirstHelper (_, StateTree v1 a1) (_, StateTree v2 a2)
  | v1 < v2 = LT
  | v1 > v2 = GT
  | v1 == v2 = EQ

{-
    *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]
pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth i (StateTree v a)
    | i > 0 = StateTree v [(a1, pruneDepth (i - 1) v1) | (a1,v1)<-a ]
    | i == 0 = StateTree v []

{-
    *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]
pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth i (StateTree v a) = StateTree v (take i [(a1, pruneBreadth i v1) | (a1,v1)<-a ])

{-
    *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]
utility :: Game -> Int
utility (Game b ps) = utilityHelp (Game b ps) [] (reachableCells b (currentCell (head ps)))

utilityHelp :: Game -> [Cell] -> [Cell] -> Int
utilityHelp (Game b ps) cpast cs
    | utilityCheckWinnings cs (head ps) = -1
    | null ([c |c <- cs, c `notElem` cpast]) = -10000
    | otherwise = -1 + maximum[utilityHelp (Game b ps) (cpast ++ [c]) (reachableCells b c)|c <- cs, c `notElem` cpast]

utilityCheckWinnings :: [Cell] -> Player -> Bool
utilityCheckWinnings cs ps = or[c `elem` (winningPositions ps)|c <- cs]


-- Lifting the utility function to work on trees.
evalTree :: GameTree -> EvalTree
evalTree = mapStateTree utility

{-
    *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]
minimaxFromTree :: EvalTree -> Action
minimaxFromTree e = minimaxFromTreeConverter (minimaxFromTreeNew e)

minimaxFromTreeNew :: EvalTree -> Result
minimaxFromTreeNew (StateTree i []) = Result i []
minimaxFromTreeNew (StateTree i as) = negResult (minimum( [ addActionToResult a (minimaxFromTreeNew e) | (a,e) <- as ] ))


minFromTree :: EvalTree -> Result
minFromTree (StateTree i []) = Result i []
minFromTree (StateTree _ as) = minimum[ addActionToResult a (maxFromTree e) | (a,e) <- as ]

maxFromTree :: EvalTree -> Result
maxFromTree (StateTree i []) = Result i []
maxFromTree (StateTree _ as) = maximum[ addActionToResult a (minFromTree e) | (a,e) <- as ]

addActionToResult :: Action -> Result -> Result
addActionToResult a (Result i as) = Result i (a : as)

minimaxFromTreeConverter :: Result -> Action
minimaxFromTreeConverter (Result i as) = head as

{-
    *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]
minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree = undefined

{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int
depth = 4

-- Given breadth for pruning.
breadth :: Int
breadth = 10

-- Function that combines all the different parts implemented in Part I.
minimax :: Game -> Action
minimax =
      minimaxFromTree -- or 'minimaxABFromTree'
    . pruneBreadth breadth
    . highFirst
    . evalTree
    . pruneDepth depth
    . generateGameTree

-- Given a game state, calls minimax and returns an action.
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax g)
    where
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c,
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }
