
% -----------------------------------------------------------------------------
% Author: Saif Mahmud
% Date: 05-22-2023
% Course: COMP 456
% Student ID: 3433058
% Puspose: Developing an expert system for solving the famous farmer, wolf, goat, cabbage problem.
% -----------------------------------------------------------------------------

% The Expert System:

:- dynamic fact/1, rule/1, askable/2.

% Loading the knowledge base file
loadKnowledgeBase :-
    writeln('Please enter the name/full path of the file containing the knowledge base:'),nl,
    writeln("For example: 'knowledge_base.pl' or 'c:\\\\Users\\\\desktop\\\\finalProject\\\\knowledge_base.pl' with the quotes."),
    
    % Read the file name from the user
    read(FileName),

    % If the user enters q, then it should not proceed
    FileName \== q,

    consult(FileName),

    % If the file does not exist, then it should not proceed
    catch(consult(FileName), Error, handleError(Error, AnyErrors)),

    % If everything appears to be fine, then proceed
    handleProceed(AnyErrors).

% If there are no errors, then proceed
handleProceed(false) :-
    nl,
    writeln('Knowledge base loaded successfully.').

% If it is a file not found error, then ask the user to enter the file name again or q to quit
handleError(error(existence_error(source_sink, InputFile), _), true) :-
    format('Error: The file "~w" does not exist. Please enter a valid file name to try again or q to quit.\n', [InputFile]),
    loadKnowledgeBase.

% If the error is unknown, print the error stack and then quit.
handleError(Error, true) :-
    format('Error: An unexpected error occurred while loading the file. Error: ~w\n', [Error]),
    respond(_,  _, quit, _).

% No errors, then set the boolan to false. False means to proceed with the program.
handle_error(_, _, false).


% Starting the expert system and loading the knowledge base
startExpertSystem :- 
    nl,
    writeln('Welcome to the Expert System!'), nl, 
    writeln('This Expert System will help you find the solution to the famous Farmer, Wolf, Goat, and Cabbage problem. The farmer wants to take them all across the river from the east shore to the west shore, but his boat is small. The boat has space for only the farmer who can row and one of the items: cabbage, wolf, or goat. The farmer cannot leave the wolf alone with the goat or the goat alone with the cabbage.'), nl,

    % Loading the knowledge base
    loadKnowledgeBase,

    % Printing the interaction information
    print_help,

    % Asking the user if they want to start the search
    askable(start, Question),
    ask(Question, Answer),
    respond(start, Question, Answer, UniqueSolutions),

    % Asking the user if they want to see the solution
    askable(see_solution, Question2),
    ask(Question2, Answer2),
    respond(see_solution, Question2, Answer2, UniqueSolutions),

    % Ending the expert system
    writeln('Thank you for using the Expert System!'), nl.

% ask(Query, Answer)
% Writes Query and reads the Answer.  Abbreviations (y, n, h, w) are
% trnslated to appropriate command be filter_abbreviations
ask(Query,Answer) :- 
    writeln(Query),
    read(A),
    filter_abbreviations(A,Answer),
    nl,
    !.

% fills in the abbreviation for the user
% filter_abbreviations(Answer, Command)
% filter_abbreviations will expand Answer into Command.
filter_abbreviations(q,quit).
filter_abbreviations(y,yes).
filter_abbreviations(n,no).
filter_abbreviations(X,X).

% Prints a help screen.
print_help :- 
    write('This Expert System allows the following responses to queries:'),nl,nl,
    write('   yes.- To Agree'),nl,
    write('   no. - To Disagree'),nl,
    write('   quit. - To Quit Expert System'),nl,
    write('   help. - prints this message.'),nl,
    write('   all commands ( except help ) may be abbreviated to first letter.'),nl,nl.

% --------------------Responding to the user--------------------

% Invalid responses are detected and the query is repeated.
respond(WantedQuery, ExplainQuestion, Bad_answer, Argument) :- 
    not(member(Bad_answer,[help,yes,no,quit])),
    write('answer must be either help, (y)es, (n)o or (q)uit'),nl,nl,
    ask(ExplainQuestion, Answer),
    respond(WantedQuery, ExplainQuestion, Answer, Argument).

% respond will process Answer (yes, no, help, quit).

% If the user enters help, then print the help screen and ask the question again
respond(WantedQuery, ExplainQuestion, help, Argument) :- 
    print_help,
    ask(ExplainQuestion, Answer),
    respond(WantedQuery, ExplainQuestion, Answer, Argument).

% If the user enters quit, then quit the program
respond(_, _, quit, _):-
    writeln('Quitting. Thank you!'), 
    halt.

% If the user enters no, when asked if they want to search for answers, then quit the program
respond(start, _, no, _) :-
    respond(_, _, quit, _).

% If the user enters yes, when asked if they want to search for answers, then proceed
respond(start, _, yes, UniqueSolutions) :-
    inferenceEngine(UniqueSolutions),
    writeln('Found the solution(s)!'), nl.

% If the user enters yes, when asked if they want to see the solutions, then proceed
respond(see_solution, _, yes, UniqueSolutions) :-
    writeln('---------Here is the first solution:-----------'), nl,
    writeNSolution(UniqueSolutions).

respond(see_solution, _, no, _).

% If the user enters yes for explanations, then proceed
respond(explain_solution, _, yes, MoveName):-
    writeExplainList(MoveName),
    write('Goal Reached!'), nl,nl.

respond(explain_solution, _, no, _).

% If the user wants to display more solutions
respond(more_solution, _, yes, T) :-
    nl,
    writeln('---------Alternative Solution:----------'), nl,
    writeNSolution(T).

% If the user does not want to display more solutions then do nothing  
respond(more_solution, _, no, _, _).


% ------------ Inference engine of the expert system ---------------

inferenceEngine(UniqueSolutions) :-

    fact(initialState(I)),
    % Finding all the solutions and storing them in a list with removing duplicates
    setof(
        (Solution, MoveName),
        depthFirstSearchHelper([I], ['Initial State'], [], Solution, MoveName),
        UniqueSolutions
    ).

% Inference engine - backward chaining implementation, using depth-first search
% Base case. If the current state is the goal state, then we have found a solution.
depthFirstSearchHelper(Path, ReverseMoveName, _, Solution, MoveName) :-
    [H|_] = Path,
    fact(goalState(H)),
    reverse(Path, Solution),
    reverse(ReverseMoveName, MoveName).

% Recursive case. If the current state is not the goal state, then we need to find a new state to move to.
depthFirstSearchHelper(Path, ReverseMoveName, Visited, RevSolution, MoveName) :- 
    [H|_] = Path,
    \+ member(H, Visited),
    rule(move(H, NewState, CurrentMove)),    
    fact(allSafeRules(NewState)),
    fact(explainSafe(NewState, NewReverseExplainList)),
    
    reverse(NewReverseExplainList, NewExplainList),
    NewReverseMoveName = [NewExplainList|ReverseMoveName],

    % Calling the helper function with the new state, move name, visited list, and solution list.
    depthFirstSearchHelper([NewState|Path], [CurrentMove|NewReverseMoveName], [H|Visited], RevSolution, MoveName).



% -------------- Print Solutions------------------

% Writing one list of states and the list of move names to the console in a readable format.

% Base case. If the list of states and the list of move names are empty, then return.
writeList([]).

%  If the last element, add goal reached.
writeList([H]) :- 
    write(H), write('.'), nl, nl,
    !.

% Recursive case. If the list of states and the list of move names are not empty, then write the state and the move name to the console.
writeList([H|T]) :-
    write(H),
    write(', '), nl, nl,
    writeList(T).


% Writing the Explanations.
% Base case. If there are no more explanations to display after this last one, then return.
writeExplainList([H|[]]) :-
    write(H), nl, nl.

% Recursive case. If there are more explanations to display, then writing the next explanation to the console.
writeExplainList([H|[T1|T2]]) :-
    write(H), write(', '), nl, nl,
    write('For my next move, I saw:'), nl,nl,
    writeList(T1),
    write('I chose: '),
    writeExplainList(T2).

% Writing only the solution in words.
% Base case. If there are no more explanations to display after this last one, then return.
writeOnlySolutionInWords([H|[]]) :-
    write(H), nl, nl.

% Recursive case. If there are more explanations to display, then writing the next explanation to the console.
writeOnlySolutionInWords([H|[_|T2]]) :-
    write(H), write(', '), nl, nl,
    writeOnlySolutionInWords(T2).

% Base case. If there are no more solutions to display, then return.
writeNSolution([]) :-
    writeln('Unfortunately there are no more solutions to display.'), nl, nl.

% Recursive case. If there are more solutions to display, then writing the next solution to the console. Also asking the user if they want to see more solutions.
writeNSolution([(Solution, MoveName)|T]) :-
    
    % Printing the states only
    write('State Transitions: '), nl, nl,
    write('Representing e = east, w = west side of the river.'), nl,
    write('State Format: '), nl,
    write('state(Farmer, Wolf, Goat, Cabbage)'), nl, nl,
    writeList(Solution),
    write('Goal Reached!'), nl,nl,nl,

    % Printing the states in words
    write('States Transition in Words:'), nl, nl,
    writeOnlySolutionInWords(MoveName),
    write('Goal Reached!'), nl,nl,nl,

    % Asking the user if they wants the explanations
    askable(explain_solution, ExplainQuestion),
    ask(ExplainQuestion, ExplainAnswer),
    respond(explain_solution, ExplainQuestion, ExplainAnswer, MoveName),
    
    nl,
    nl,

    % Asking the user if they want to see more solutions
    askable(more_solution, Question),
    ask(Question, Answer),
    respond(more_solution, Question, Answer, T).
