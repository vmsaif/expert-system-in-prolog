
% Farmer moves with goat
rule(
    move(state(F1, W1, G1, C1), state(F2, W2, G2, C2), 'Farmer moves with the goat')) :-
    F1 = G1,
    fact(changeDirection(F1, F2)),
    W1 = W2,
    fact(changeDirection(G1, G2)),
    C1 = C2. 

% Farmer moves with wolf
rule(
    move(state(F1, W1, G1, C1), state(F2, W2, G2, C2), 'Farmer moves with the wolf')) :-
    F1 = W1,
    fact(changeDirection(F1, F2)),
    fact(changeDirection(W1, W2)),
    G1 = G2,
    C1 = C2.

% Farmer moves with cabbage
rule(
    move(state(F1, W1, G1, C1), state(F2, W2, G2, C2), 'Farmer moves with the cabbage')) :-
    F1 = C1,
    fact(changeDirection(F1, F2)),
    W1 = W2,
    G1 = G2,
    fact(changeDirection(C1, C2)).

% Farmer moves alone
rule(move(state(F1, W1, G1, C1), state(F2, W2, G2, C2), 'Farmer comes back')) :-
    fact(changeDirection(F1, F2)),
    G1 = G2,
    W1 = W2,
    C1 = C2.

% The starting state of the farmer, wolf, goat, and cabbage
fact(initialState(state(e, e, e, e))).

% The goal state of the farmer, wolf, goat, and cabbage
fact(goalState(state(w, w, w, w))).

% It is used to change the direction of the farmer, wolf, goat, and cabbage from e to w and vice versa.
fact(changeDirection(e, w)).
fact(changeDirection(w, e)).

% The farmer, wolf, goat, and cabbage are safe if they are on the same side or the wolf and goat are not together without the farmer or the goat and cabbage are not together without the farmer.
fact(allSafeRules(state(Farmer, Wolf, Goat, Cabbage))) :-
    (
        % all are on the same side
        (Farmer == Wolf, Farmer == Goat, Farmer == Cabbage);

        % wolf and goat are not together or farmer is with them
        (Wolf \= Goat; Farmer = Wolf, Farmer = Goat),
        
        % goat and cabbage are not together or farmer is with them
        (Goat \= Cabbage ; Farmer = Goat, Farmer = Cabbage)
    ).

% The explaination of why the item is safe.
fact(explainSafe(state(Farmer, Wolf, Goat, Cabbage), NewReverseExplainList)) :-
    (
        % all are on the same side
        (
            (Farmer == Wolf, Farmer == Goat, Farmer == Cabbage)
            ,
            NewReverseExplainList = ['-- All will be on the same side in my next move, So the goat and cabbage will be safe'], 
            !
        )
        ;

        % wolf and goat are not together or farmer is with them
        (
            (
                (
                    Wolf \= Goat
                    ,
                    TempList1 = ['-- The Wolf and goat will not together in my next move, so they will be safe']
                )
                ; 
                
                (
                    Farmer = Wolf, 
                    Farmer = Goat
                    ,
                    TempList1 = ['-- Farmer will be with the Wolf and Goat in my next move, so they will be safe']
                )
            )
            
            ,

            % goat and cabbage are not together or farmer is with them
            (
                (
                    Goat \= Cabbage
                    ,
                    append(TempList1, ['-- Goat and Cabbage will not together in my next move, so they will be safe'], TempList2)
                )
                ; 
                (
                    Farmer = Goat, 
                    Farmer = Cabbage
                    , 
                    append(TempList1, ['-- Farmer will be with the Goat and Cabbage in my next move, so they will be safe'], TempList2)
                )
            )
            ,
            NewReverseExplainList = TempList2,
            !
        )
    ).
    
% Askables
askable(start, 'Start the search? (y/n) or q to Quit').

askable(see_solution, 'Do you want to see the solution? (y/n) or q to Quit').

askable(more_solution, 'Do you want to see more solution? (y/n) or q to Quit').

askable(explain_solution, 'Do you want to see the explanation of the solution? (y/n) or q to Quit').