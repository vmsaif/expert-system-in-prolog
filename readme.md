# Expert System in Prolog

This expert system is designed to solve the famous Farmer, Wolf, Goat, and Cabbage problem. The farmer wants to take them all across the river from the east shore to the west shore, but his boat is small. The boat has space for only the farmer who can row and one of the items: cabbage, wolf, or goat. The farmer cannot leave the wolf alone with the goat or the goat alone with the cabbage.

It is implimented using the prolog programming language which is a logic programming language associated with artificial intelligence and logic programming.

## Execution Instructions:

In the terminal, write 
```
swipl
```
then press enter.

Then, load the expertSystem.pl file into the prolog environment. The user can use the following command to load the file:

```
consult('expertSystem.pl').
```

Next, in the console, the user should type the following command to start the expert system:

```
startExpertSystem.
```

The system will provide greetings and then will ask for the path of knowledge base file. The user can enter the name or full path of the file containing the knowledge base. The full path is recommended. Please use the single quotes around the path. For example:

```
Please enter the name/full path of the file containing the knowledge base:

For example: 'knowledge_base.pl'. or 'c:\\Users\\desktop\\finalProject\\knowledge_base.pl'. with the quotes and dot.
|: 
```
MAKE SURE TO ADD THE PERIOD AT THE END OF THE PATH TO EXECUTE THE COMMAND (the normal prolog way).

For Windows users, the user should provide the path of the knowledge base file. For example:
```
'C:\\\\Users\\\\vmsai\\\\Documents\\\\COMP456\\\\FinalProject\\\\knowledge_base.pl'.
```
The file is then loaded into the prolog environment.

From then on, the system will start querying for the next step. The user can enter `y` to start the search or `n`. To quit the expert system. The user can also enter `q` to quit or help to see the instructions in the expert system at any time.


## Implimentation:

* Rules and Facts (Knowledge Base): In the knowledge_base.pl file, the rules and facts are defined. The rules are used to define the state transitions and the facts are used to define the initial state and the goal state. This is what we know of the world and the rules that govern it.
* Starting the Expert System: The user is prompted to start the search. The user can enter y to start the search or n to quit the expert system. The user can also enter q to quit or help to see the instructions in the expert system at any time.
* Loading the Knowledge Base: The knowledge base is loaded into the prolog environment using the consult predicate. The user is prompted to enter the name or full path of the file containing the knowledge base. The file is then loaded into the prolog environment.
* Interacting with the User (User Interface): The user is asked sevaral questions to determine if the user wants to see the solution, the explanation of the solution, or more solutions. The user can enter y to see the solution, the explanation of the solution, or more solutions. The user can also enter n to not see the solution, the explanation of the solution, or more solutions. The user can also enter q to quit the expert system or help to see the instructions in the expert system at any time.
* Inference Engine: This is the core of the Expert System. The engine uses depth first search algorithm to find all possible solutions to the problem. It uses the rules and facts defined in the knowledge base to find the solutions. The algorithm uses the state transitions to find the next state and the goal state to determine if the goal is reached. It track down all the names of the moves and the proper explanations of each move. The engine uses data-driven method to find the solutions. It starts with the known data, the initial state, and then uses the rules to infer new data, the next state, until the goal state is reached. The engine uses backtracking to find all possible solutions. It backtracks to the previous state when it reaches a dead end. It then tries the next possible state. It continues this process until all possible solutions are found.
* Printing Solutions: The system provides detailed explanations of each solution using the `writeList`, `writeExplainList`, `writeOnlySolutionInWords`, and `writeNSolution` predicates. This provides an understanding of the solution's steps and the logic behind each step.

## How the system works:

* When the user starts the expert system, they are welcomed and presented with a brief description of the problem. 
* The system then asks the user to enter the name or full path of the file containing the knowledge base. The user can enter the name or full path of the file containing the knowledge base. The file is then loaded into the prolog environment.
* The system then asks the user if they want to start the search. The user can enter y to start the search or n to quit the expert system. The user can also enter q to quit or help to see the instructions in the expert system at any time.
* The system then finds all possible solutions to the problem. It uses the rules and facts defined in the knowledge base to find the solutions. The algorithm uses the state transitions to find the next state and the goal state to determine if the goal is reached. It track down all the names of the moves and the proper explanations of each move. 
* The user is then asked if they want to see the solution. Also they are asked if they want the explanation of how the solution was derived. The user can enter y to see the solution, the explanation of the solution, or more solutions. Then the system provides a step by step solution, and explanation. The user can also enter n to not agree.
* Lastly, the user is asked if they want to see alternative solutions. If available, the system will provide it.

In summary, the expert system efficiently solves the problem using depth-first search and provides a detailed, step-by-step explanation of each solution, making it an effective tool for understanding the logic behind solving the Farmer, Wolf, Goat, and Cabbage problem.

## Output Example:

```
?- startExpertSystem.

Welcome to the Expert System!

This Expert System will help you find the solution to the famous Farmer, Wolf, Goat, and Cabbage problem. The farmer wants to take them all across the river from the east shore to the west shore, but his boat is small. The boat has space for only the farmer who can row and one of the items: cabbage, wolf, or goat. The farmer cannot leave the wolf alone with the goat or the goat alone with the cabbage.

Please enter the name/full path of the file containing the knowledge base:

For example: 'knowledge_base.pl' or 'c:\\Users\\desktop\\finalProject\\knowledge_base.pl' with the quotes.
|: 'C:\\\\Users\\\\vmsai\\\\Documents\\\\GitHub\\\\COMP456\\\\FinalProject2\\\\knowledge_base.pl'.

Knowledge base loaded successfully.
This Expert System allows the following responses to queries:

   yes.- To Agree
   no. - To Disagree
   quit. - To Quit Expert System
   help. - prints this message.
   all commands ( except help ) may be abbreviated to first letter.

Start the search? (y/n) or q to Quit
|: y.

Found the solution(s)!

Do you want to see the solution? (y/n) or q to Quit
|: y.

---------Here is the first solution:-----------

State Transitions: 

Representing e = east, w = west side of the river.
State Format: 
state(Farmer, Wolf, Goat, Cabbage)

state(e,e,e,e), 

state(w,e,w,e), 

state(e,e,w,e), 

state(w,e,w,w), 

state(e,e,e,w), 

state(w,w,e,w), 

state(e,w,e,w), 

state(w,w,w,w).

Goal Reached!


States Transition in Words:

Initial State,

Farmer moves with the goat,

Farmer comes back,

Farmer moves with the cabbage,

Farmer moves with the goat,

Farmer moves with the wolf,

Farmer comes back,

Farmer moves with the goat

Goal Reached!


Do you want to see the explanation of the solution? (y/n) or q to Quit
|: y.

Initial State,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer moves with the goat,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer comes back,

For my next move, I saw:

-- Farmer will be with the Goat and Cabbage in my next move, so they will be safe,    

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer moves with the cabbage,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- Farmer will be with the Wolf and Goat in my next move, so they will be safe.       

I chose: Farmer moves with the goat,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer moves with the wolf,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer comes back,

For my next move, I saw:

-- All will be on the same side in my next move, So the goat and cabbage will be safe.

I chose: Farmer moves with the goat

Goal Reached!



Do you want to see more solution? (y/n) or q to Quit
|: y.


---------Alternative Solution:----------

State Transitions:

Representing e = east, w = west side of the river.
State Format:
state(Farmer, Wolf, Goat, Cabbage)

state(e,e,e,e),

state(w,e,w,e),

state(e,e,w,e),

state(w,w,w,e),

state(e,w,e,e),

state(w,w,e,w),

state(e,w,e,w),

state(w,w,w,w).

Goal Reached!


States Transition in Words:

Initial State,

Farmer moves with the goat,

Farmer comes back,

Farmer moves with the wolf,

Farmer moves with the goat,

Farmer moves with the cabbage,

Farmer comes back,

Farmer moves with the goat

Goal Reached!


Do you want to see the explanation of the solution? (y/n) or q to Quit
|: y.

Initial State,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer moves with the goat,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer comes back,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- Farmer will be with the Wolf and Goat in my next move, so they will be safe.       

I chose: Farmer moves with the wolf,

For my next move, I saw:

-- Farmer will be with the Goat and Cabbage in my next move, so they will be safe,    

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer moves with the goat,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer moves with the cabbage,

For my next move, I saw:

-- Goat and Cabbage will not together in my next move, so they will be safe,

-- The Wolf and goat will not together in my next move, so they will be safe.

I chose: Farmer comes back,

For my next move, I saw:

-- All will be on the same side in my next move, So the goat and cabbage will be safe.

I chose: Farmer moves with the goat

Goal Reached!



Do you want to see more solution? (y/n) or q to Quit
|: y.


---------Alternative Solution:----------

Unfortunately there are no more solutions to display.


Thank you for using the Expert System!
true .

3 ?-
```
