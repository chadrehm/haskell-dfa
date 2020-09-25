type State = [Char]
type Label = Char
type Transition = (State, Label, State)
type DFA = ([State], [Label], State, [State], [State], [Transition])

dfaStateFactory :: DFA
dfaStateFactory = (["q0","q1","q2","q3","q4"],
       ['0','1','.'],
       "q0",
       ["q3"],
       ["q0"],
       [("q0",'0',"q1"),("q0",'1',"q1"),("q0",'.',"q2"),
        ("q1",'0',"q1"),("q1",'1',"q1"),("q1",'.',"q3"),
        ("q2",'0',"q3"),("q2",'1',"q3"),("q2",'.',"q4"),
        ("q3",'0',"q3"),("q3",'1',"q3"),("q3",'.',"q4"),
        ("q4",'0',"q4"),("q4",'1',"q4"),("q4",'.',"q4")])

allStates :: DFA -> [State]
allStates ( item, _, _, _, _, _ ) = item
alphabet :: DFA -> [Label]
alphabet ( _, item, _, _, _, _) = item
firstState :: DFA -> State
firstState ( _, _, item, _, _, _) = item
acceptStates :: DFA -> [State]
acceptStates ( _, _,  _, item, _, _) = item
currentState ( _, _,  _, _, item, _) = item
allTransitions :: DFA -> [Transition]
allTransitions ( _, _,  _, _, _, item ) = item

transFromState :: (a, b, c) -> a
transFromState (item, _, _ ) = item
transLabel :: (a, b, c) -> b
transLabel ( _, item, _ ) = item
transToState :: (a, b, c) -> c
transToState ( _, _, item ) = item

findTransition :: State -> Label -> [Transition] -> [Transition]
findTransition currentState transitionLabel transitionList = [ trans | trans <- transitionList, transFromState trans == currentState && transLabel trans == transitionLabel] 

findNextState :: DFA -> Char -> State
findNextState dfa input = transToState (head (findTransition (head (currentState dfa)) input (allTransitions dfa)))

dfaAccept :: DFA -> String -> Bool
dfaAccept dfa inputString
    | inputString == [] = False
    | length inputString == 1 && currentState `elem` (acceptStates dfa) = True
    | length inputString == 1 && not (currentState `elem` (acceptStates dfa)) = False
    | length inputString > 0 = dfaAccept (updateDfa dfa currentState) (tail inputString)
    where currentState = findNextState dfa (head inputString)

update :: (a1, b, c, d, [a2], f) -> a2 -> [a2]
update dfa input = [input] ++ (currentState dfa)

updateDfa :: DFA -> State -> DFA
updateDfa dfa input = (allStates dfa, alphabet dfa, firstState dfa, acceptStates dfa, update dfa input, allTransitions dfa)