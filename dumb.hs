data Chunk = Chunk String Bool
type Regex = [Chunk]

type Index = Int
type TransitionPredicate = Char -> Bool
data Transition = Transition Index TransitionPredicate
data State = State [Transition] Bool Bool
type StateMachine = [State]

offset :: StateMachine -> Int -> StateMachine
offset machine delta =
    let offsetTransition (Transition index predicate) =
            Transition (index + delta) predicate
        offsetState (State transitions starting ending) =
            State (map offsetTransition transitions) starting ending in
    map offsetState machine

stitchTransitions :: StateMachine -> [Index] -> [Index] 
        
empty :: StateMachine
empty = [State [] True True]

singleCharMatcher :: TransitionPredicate -> StateMachine
singleCharMatcher predicate =
    [State [Transition 1 predicate] True False,State [] False True]
                     
dot :: StateMachine
dot = singleCharMatcher $ const True

literal :: Char -> StateMachine
literal char = singleCharMatcher (== char)

alt :: StateMachine -> StateMachine -> StateMachine
alt machine1 machine2 = machine1 ++ offset machine2 (length machine1)

concat :: StateMachine -> StateMachine -> StateMachine
concat first second =
