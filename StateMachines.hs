module StateMachines
    (StateMachine(..), TransitionPredicate, empty, singleCharMatcher, alt, cat,
     kleene, matches, anyChar, specificChar, oneOrMore, optional, repeatFixed,
     repeatAtLeast, repeatBounded, Transition(..)) where

import qualified Data.Set as Set

type Index = Int
type TransitionPredicate = Char -> Bool
data Transition = Transition Index TransitionPredicate
type IndexSet = Set.Set Index

instance Show Transition where
    show (Transition index _) = '`':show index
                  
type State = [Transition]
data StateMachine = StateMachine [State] IndexSet IndexSet deriving Show

combineStateLists :: [State] -> [State] -> [State]
combineStateLists states1 states2 =
    let delta = length states1
        offsetTransition (Transition index predicate) =
            Transition (index + delta) predicate in
    states1 ++ map (map offsetTransition) states2

offsetIndexSet :: Int -> IndexSet -> IndexSet
offsetIndexSet delta = Set.mapMonotonic (+ delta)

empty :: StateMachine
empty = StateMachine [[]] (Set.singleton 0) (Set.singleton 0)

singleCharMatcher :: TransitionPredicate -> StateMachine
singleCharMatcher predicate =
    StateMachine [[Transition 1 predicate],[]]
                                (Set.singleton 0) (Set.singleton 1)

alt :: StateMachine -> StateMachine -> StateMachine
alt (StateMachine states1 start1 final1) (StateMachine states2 start2 final2) =
    let delta = length states1 in
    StateMachine (combineStateLists states1 states2)
                     (Set.union start1 $ offsetIndexSet delta start2)
                     (Set.union final1 $ offsetIndexSet delta final2)

addEpsilons :: [State] -> IndexSet -> IndexSet -> [State]
addEpsilons states sources destinations =
    let isFromSource (Transition index _) = Set.member index sources
        replaceTransition (Transition index predicate) destination =
            Transition destination predicate
        newTransitions transition =
            map (replaceTransition transition) $ Set.toList destinations
        addToStates state =
            state ++ (concat $ map newTransitions $ filter isFromSource state) in
    map addToStates states

cat :: StateMachine -> StateMachine -> StateMachine
cat (StateMachine states1 start1 final1) (StateMachine states2 start2 final2) =
    let delta = length states1
        newStart2 = offsetIndexSet delta start2
        newStates = addEpsilons (combineStateLists states1 states2)
                    final1 newStart2 in
    StateMachine newStates
                 (if Set.null $ Set.intersection start1 final1 then start1
                  else Set.union start1 newStart2)
                 (offsetIndexSet delta final2)

kleene :: StateMachine -> StateMachine
kleene (StateMachine states start final) =
    StateMachine (addEpsilons states final start) start start

matches :: StateMachine -> String -> Bool
matches (StateMachine states start final) str =
    let canTransit char (Transition _ predicate) = predicate char
        newIndex (Transition index _) = index
        statesReachableFrom char index =
            Set.fromList $ map newIndex $ filter (canTransit char) $
               states !! index
        currentIndices previousIndices char =
            Set.unions $ map (statesReachableFrom char) $
               Set.toList previousIndices in
    not $ Set.null $ Set.intersection final $ foldl currentIndices start str

anyChar :: StateMachine
anyChar = singleCharMatcher $ const True

specificChar :: Char -> StateMachine
specificChar char = singleCharMatcher (== char)

oneOrMore :: StateMachine -> StateMachine
oneOrMore machine = cat machine $ kleene machine

optional :: StateMachine -> StateMachine
optional = alt empty

repeatFixed :: Int -> StateMachine -> StateMachine
repeatFixed count machine = foldl cat empty $ replicate count machine

repeatAtLeast :: Int -> StateMachine -> StateMachine
repeatAtLeast count machine = cat (repeatFixed count machine) $ kleene machine

repeatBounded :: Int -> Int -> StateMachine -> StateMachine
repeatBounded lowerCount upperCount machine =
    let addRepetition existing = optional $ cat machine existing in
    cat (repeatFixed lowerCount machine) $
        iterate addRepetition empty !! (upperCount - lowerCount)
