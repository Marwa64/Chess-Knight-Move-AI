% ['D:/Marwa/1. University Folder/Artificial Intelligence/Assignment 3/Chess Knight Move.pl'].

test:-play([0,0],[6,6], [[1,4] , [2,2], [2,6], [4,4],[5,1],[7,3]]).

%problem specific part

% Knight Piece
piece(X, Y, _, [GoalX, GoalY], _, 3):-
X == GoalX,
Y == GoalY.

% Goal Piece
piece(X, Y, [KnightX, KnightY], _, _, 2):-
X == KnightX,
Y == KnightY.

% Obstacle Piece
piece(X, Y, _, _, Obstacles, 1):-
member([X,Y], Obstacles).

% Empty Space
piece(_, _, _, _, _, 0).

incrementX(X, Y, NewX):-
Y =< 7,
X < 7,
NewX is X + 1.

incrementY(X, Y, NewY):-
X =< 7,
Y < 7,
NewY is Y + 1.

resetIndexY(X, Y, 0):-
X =< 7,
Y == 7.

resetIndexY(_, Y, Y).

% Generating the grid
getGrid(_, 7, 7, Goal, Knight, Obstacles, [X]):-
piece(7, 7, Knight, Goal, Obstacles, X).

getGrid(Initial, Index_X, Index_Y, Goal, Knight, Obstacles, Grid):-
      Index_Y < 7,
      incrementY(Index_X, Index_Y, NewIndex_Y),
      getGrid(Initial, Index_X, NewIndex_Y, Goal, Knight, Obstacles, NewGrid),
      piece(Index_X, Index_Y, Knight, Goal, Obstacles, Piece),
      append([Piece], NewGrid, Grid).

getGrid(Initial, Index_X, Index_Y, Goal, Knight, Obstacles, Grid):-
      Index_X < 7,
      incrementX(Index_X, Index_Y, NewIndex_X),
      resetIndexY(Index_X, Index_Y, NewIndex_Y),
      getGrid(Initial, NewIndex_X, NewIndex_Y, Goal, Knight, Obstacles, NewGrid),
      piece(Index_X, Index_Y, Knight, Goal, Obstacles, Piece),
      append([Piece], NewGrid, Grid).



% Printing the Grid
printGrid(_, 64, _):- !.

printGrid(Grid, Index, BreakLine):-
		Index == BreakLine,
		write('\n'),
		BreakLine2 is BreakLine + 8,
		printGrid(Grid, Index, BreakLine2).

printGrid(Grid, Index, BreakLine):-
		Index < 64,
		nth0(Index, Grid, X),
		write(X),
		write('  '),
		NewIndex is Index + 1,
		printGrid(Grid, NewIndex, BreakLine).


replace([_|T],0,E,[E|T]).
replace([H|T],P,E,[H|R]) :-
    P > 0, NP is P-1, replace(T,NP,E,R).


side(Current, Index):-
Index =< 64,
Current == Index,!.

side(Current, Index):-
Index =< 64,
Current \== Index,
NewIndex is Index + 8,
side(Current, NewIndex),!.

onTheRight(Current, Spaces):-
NewSpaces is -1 * Spaces,
side(Current, NewSpaces).

onTheLeft(Current, Spaces):-
NewSpaces is Spaces-1,
side(Current, NewSpaces).

atTheTop(Current, Spaces):-
Current =< (8 * Spaces) - 1.

atTheBottom(Current, Spaces):-
Current > 64 - (8 * Spaces).


% 2 down then 1 right
move(State, NextState):-
	nth0(KnightIndex, State, 2),
	not(atTheBottom(KnightIndex, 2)),
	not(onTheRight(KnightIndex, 1)),
	Temp is KnightIndex + (8 * 2),
	NextIndex is Temp + 1,
	nth0(NextIndex, State, NextVal),
	(NextVal == 0;
	NextVal == 3),
	replace(State, NextIndex, 2, NewState),
	replace(NewState, KnightIndex, 0, NextState).

% 2 down then 1 left
move(State, NextState):-
	nth0(KnightIndex, State, 2),
	not(atTheBottom(KnightIndex, 2)),
	not(onTheLeft(KnightIndex, 1)),
	Temp is KnightIndex + (8 * 2),
	NextIndex is Temp - 1,
	nth0(NextIndex, State, NextVal),
	(NextVal == 0;
	NextVal == 3),
	replace(State, NextIndex, 2, NewState),
	replace(NewState, KnightIndex, 0, NextState).

% 2 right then 1 down
move(State, NextState):-
	nth0(KnightIndex, State, 2),
	not(onTheRight(KnightIndex, 2)),
	not(atTheBottom(KnightIndex, 1)),
	Temp is KnightIndex + 2,
	NextIndex is Temp + 8,
	nth0(NextIndex, State, NextVal),
	(NextVal == 0;
	NextVal == 3),
	replace(State, NextIndex, 2, NewState),
	replace(NewState, KnightIndex, 0, NextState).

% 2 left then 1 down
move(State, NextState):-
	nth0(KnightIndex, State, 2),
	not(onTheLeft(KnightIndex, 2)),
	not(atTheBottom(KnightIndex, 1)),
	Temp is KnightIndex - 2,
	NextIndex is Temp + 8,
	nth0(NextIndex, State, NextVal),
	(NextVal == 0;
	NextVal == 3),
	replace(State, NextIndex, 2, NewState),
	replace(NewState, KnightIndex, 0, NextState).

% 2 up then 1 left
move(State, NextState):-
	nth0(KnightIndex, State, 2),
	not(atTheTop(KnightIndex, 2)),
	not(onTheLeft(KnightIndex, 1)),
	Temp is KnightIndex - (8 * 2),
	NextIndex is Temp - 1,
	nth0(NextIndex, State, NextVal),
	(NextVal == 0;
	NextVal == 3),
	replace(State, NextIndex, 2, NewState),
	replace(NewState, KnightIndex, 0, NextState).

% 2 up then 1 right
move(State, NextState):-
	nth0(KnightIndex, State, 2),
	not(atTheTop(KnightIndex, 2)),
	not(onTheRight(KnightIndex, 1)),
	Temp is KnightIndex - (8 * 2),
	NextIndex is Temp + 1,
	nth0(NextIndex, State, NextVal),
	(NextVal == 0;
	NextVal == 3),
	replace(State, NextIndex, 2, NewState),
	replace(NewState, KnightIndex, 0, NextState).

% 2 right then 1 up
move(State, NextState):-
	nth0(KnightIndex, State, 2),
	not(onTheRight(KnightIndex, 2)),
	not(atTheTop(KnightIndex, 1)),
	Temp is KnightIndex + 2,
	NextIndex is Temp - 8,
	nth0(NextIndex, State, NextVal),
	(NextVal == 0;
	NextVal == 3),
	replace(State, NextIndex, 2, NewState),
	replace(NewState, KnightIndex, 0, NextState).

% 2 left then 1 up
move(State, NextState):-
	nth0(KnightIndex, State, 2),
	not(onTheLeft(KnightIndex, 2)),
	not(atTheTop(KnightIndex, 1)),
	Temp is KnightIndex - 2,
	NextIndex is Temp - 8,
	nth0(NextIndex, State, NextVal),
	(NextVal == 0;
	NextVal == 3),
	replace(State, NextIndex, 2, NewState),
	replace(NewState, KnightIndex, 0, NextState).

% end of specific part


%general algorithm

play(Knight, Goal, Obstacles):-
	getGrid([], 0, 0, Goal, Knight, Obstacles, Grid),
	nth0(KnightIndex, Grid, 2),
	nth0(GoalIndex, Grid, 3),
	replace(Grid, KnightIndex, 0, NewGrid),
	replace(NewGrid, GoalIndex, 2, NewGoal),
	getHeuristic(Grid, GoalIndex, H),
	path([[Grid,null, 0, H, H]], GoalIndex, [],NewGoal),!.


%main predicate that takes open list, closed list and goal state

path([],_, _, _):-
		write('No solution'),nl,!.

path(Open, _, Closed, Goal):-
		getBestChild(Open, [Goal, Parent, PC, H, TC], _),
		write('A solution is found'),  nl ,
		printsolution([Goal,Parent, PC, H, TC], Closed),!.

path(Open, GoalIndex, Closed, Goal):-
		getBestChild(Open, [State, Parent, PC, H, TC], RestOfOpen),
		getchildren(State, GoalIndex, Open, Closed, Children, PC, Goal),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, GoalIndex, [[State, Parent, PC, H, TC] | Closed], Goal).


getchildren(State, GoalIndex, Open ,Closed , Children, PC, Goal):-
		bagof(X, moves( State, GoalIndex, Open, Closed, X, PC, Goal), Children) .
getchildren(_,_,_, [],_,_).


addListToOpen(Children, [], Children).

addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).


getBestChild([Child], Child, []).

getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromList(Best, Open, RestOpen).



getBestChild1([State], State).

getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).


getBest([State, Parent, PC, H, TC], [_, _, _, _, TC1], [State, Parent, PC, H, TC]):-
	TC < TC1, !.
getBest([_, _, _, _, _], [State1, Parent1, PC1, H1, TC1], [State1, Parent1, PC1, H1, TC1]).


removeFromList(_, [], []).

removeFromList(H, [H|T], V):-
	!, removeFromList(H, T, V).

removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).


moves( State, GoalIndex, Open, Closed,[Next,State, NPC, H, TC], PC, _):-
		move(State,Next),
		\+ member([Next, _, _, _, _],Open),
		\+ member([Next, _, _, _, _],Closed),
		NPC is PC + 1,
		getHeuristic(Next, GoalIndex, H),
		TC is NPC + H.



%calculate heuristic of some state

%here it is calculated as number of spaces between the knight and the goal
absVal(Val, R):-
Val < 0,
R is Val * -1, !.

absVal(Val, Val):-!.

getHeuristic(State, GoalIndex, H):-
	nth0(KnightIndex, State, 2),
	Spaces is (GoalIndex - KnightIndex) / 3,
	absVal(Spaces, H).

%prints the path from start state to goal state

printsolution([State, null, _, _, _],_):-
		printGrid(State, 0, 8), write("\n\n").

printsolution([State, Parent, _, _, _], Closed):-
		member([Parent, GrandParent, PC1, H1, TC1], Closed),
		printsolution([Parent, GrandParent, PC1, H1, TC1], Closed),
		printGrid(State, 0, 8), 
		write("\n\n").