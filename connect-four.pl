%Test computer
player(1, computer).
player(2, human).

player_mark(1, 'x').
player_mark(2, 'o').

maximizing('x').      %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').     %%% the player playing o is always trying to minimize the utility of the board position

board([['o','x','e','e','e','x'],
       ['o','x','x','x','e','e'],
       ['e','e','e','e','e','o'],
       ['x','e','e','e','e','o'],
       ['x','x','o','x','o','o'],
       ['o','o','x','e','e','e'],
       ['o','x','x','o','e','e']]).
%---------------

% Each array on the matrix is a column of the board from bottom to top.
empty_mark('e').
empty_board([['e','e','e','e','e','e'], ['e','e','e','e','e','e'], ['e','e','e','e','e','e'],
             ['e','e','e','e','e','e'], ['e','e','e','e','e','e'], ['e','e','e','e','e','e'],
             ['e','e','e','e','e','e']]).

full_board([['x','x','x','x','x','x'], ['x','x','x','x','x','x'], ['x','x','x','x','x','x'],
             ['x','x','x','x','x','x'], ['x','x','x','x','x','x'], ['x','x','x','x','x','x'],
             ['x','x','x','x','x','x']]).



next_player(1,2).
next_player(2,1).


inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').
%---------------

column([C,_,_,_,_,_,_], 1, C).
column([_,C,_,_,_,_,_], 2, C).
column([_,_,C,_,_,_,_], 3, C).
column([_,_,_,C,_,_,_], 4, C).
column([_,_,_,_,C,_,_], 5, C).
column([_,_,_,_,_,C,_], 6, C).
column([_,_,_,_,_,_,C], 7, C).

square([M,_,_,_,_,_],1,M).
square([_,M,_,_,_,_],2,M).
square([_,_,M,_,_,_],3,M).
square([_,_,_,M,_,_],4,M).
square([_,_,_,_,M,_],5,M).
square([_,_,_,_,_,M],6,M).




playable_square(C,N) :-
    findall(NE, square(C, NE, 'e'), L),
    min_list(L,N)
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run :-
    empty_board(B),
    %read_players,
    play(1,B).

move(B, IC, M, B2) :-
    column(B, IC, C),
    playable_square(C,N),
    replace_item(C, N, M, C2),
    replace_item(B, IC, C2, B2)
	.

play(P, B) :-
    print_board(B),
    make_move(P, B, B2),
    (   game_over(P, B2) ->
        player_mark(P, M),
        output_winner(B2)
    ;   
        next_player(P, P2),
        play(P2, B2)
    ).


make_move(P, B, B2) :-
    player(P, Type),
    make_move2(Type, P, B, B2)
    .

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Player '),
    write(P),
    write(' move? '),
    read(CN),
    
    column(B,CN,C),
    empty_mark(E),
    player_mark(P, M),
    move(B,CN, M, B2), !
    .
    
make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Please select a numbered column.'),
    make_move2(human,P,B,B2)
    .

make_move2(computer, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    minimax(0, B, M, S, U),
    move(B,S,M,B2),

    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in square '),
    write(S),
    write('.')
    .


read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% On verifie si un joueur a gagné la partie %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TO TEST THIS FILE: 
/*
trace, (
([[z,_,z,z,_,_], 
             	[z,z,_,z,_,_], 
            	[z,_,z,_,_,_],
             	[_,z,_,_,_,_],
             	[z,_,_,_,_,_], 
             	[_,_,_,_,_,_],
             	[_,_,_,_,_,_]], player1)).
*/

% Les positions gangnats sur:
%       - Une collone
winner(Board, M) :-
    winingInAColumn(Board, M),!.

%       - Une ligne
winner(Board, M) :-
    winingInARow(Board, M),!.

%       - Une diagonal descendant
winner(Board, M) :-
    winingInADiagonalDesc(Board, M),!.

%       - Une diagonal ascendant
winner(Board, M) :-
    winingInADiagonalAsc(Board, M),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% On verifie si un joueur a gagné la partie sur %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%       - Une collone
winingInAColumn([Column|_], M) :-
    winingColumn(Column, M),!.
winingInAColumn([_|Rest], M) :-
    winingInAColumn(Rest, M).

%       - Une ligne
winingInARow([[A|_], [B|_], [C|_], [D|_], [E|_], [F|_], [G|_]], M) :-
    winingRow([A,B,C,D,E,F,G], M),!.
winingInARow([_|A1], [_,B1], [_,C1], [_,D1], [_,E1], [_,F1], [_,G1], M) :-
    winingInARow([A1, B1, C1, D1, E1, F1, G1], M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% On define les positions gangnats pour: %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%       - Une collone:

winingColumn([M,M,M,M,_,_], M).
winingColumn([_,M,M,M,M,_], M).
winingColumn([_,_,M,M,M,M], M).

%       - Une ligne:
winingRow([M,M,M,M,_,_,_], M) .
winingRow([_,M,M,M,M,_,_], M) .
winingRow([_,_,M,M,M,M,_], M) .
winingRow([_,_,_,M,M,M,M], M) .

%       - Une diagonal descendant:
winingInADiagonalDesc([[M,_,_,_,_,_],[_,M,_,_,_,_],[_,_,M,_,_,_],[_,_,_,M,_,_],_,_,_], M).
winingInADiagonalDesc([[_,M,_,_,_,_],[_,_,M,_,_,_],[_,_,_,M,_,_],[_,_,_,_,M,_],_,_,_], M).
winingInADiagonalDesc([[_,_,M,_,_,_],[_,_,_,M,_,_],[_,_,_,_,M,_],[_,_,_,_,_,M],_,_,_], M).
winingInADiagonalDesc([_,[M,_,_,_,_,_],[_,M,_,_,_,_],[_,_,M,_,_,_],[_,_,_,M,_,_],_,_], M).
winingInADiagonalDesc([_,[_,M,_,_,_,_],[_,_,M,_,_,_],[_,_,_,M,_,_],[_,_,_,_,M,_],_,_], M).
winingInADiagonalDesc([_,[_,_,M,_,_,_],[_,_,_,M,_,_],[_,_,_,_,M,_],[_,_,_,_,_,M],_,_], M).

winingInADiagonalDesc([_,_,[M,_,_,_,_,_],[_,M,_,_,_,_],[_,_,M,_,_,_],[_,_,_,M,_,_],_], M).
winingInADiagonalDesc([_,_,[_,M,_,_,_,_],[_,_,M,_,_,_],[_,_,_,M,_,_],[_,_,_,_,M,_],_], M).
winingInADiagonalDesc([_,_,[_,_,M,_,_,_],[_,_,_,M,_,_],[_,_,_,_,M,_],[_,_,_,_,_,M],_], M).
winingInADiagonalDesc([_,_,_,[M,_,_,_,_,_],[_,M,_,_,_,_],[_,_,M,_,_,_],[_,_,_,M,_,_]], M).
winingInADiagonalDesc([_,_,_,[_,M,_,_,_,_],[_,_,M,_,_,_],[_,_,_,M,_,_],[_,_,_,_,M,_]], M).
winingInADiagonalDesc([_,_,_,[_,_,M,_,_,_],[_,_,_,M,_,_],[_,_,_,_,M,_],[_,_,_,_,_,M]], M).
%       - Une diagonal ascendant:
winingInADiagonalAsc(Board,M) :-
    reverse(Board, InversedBoard), 
    winingInADiagonalDesc(InversedBoard, M)
    .

%set_players(0) :- 
%    asserta( player(1, computer) ),
%    asserta( player(2, computer) ), !
%    .
%
%set_players(1) :-
%    nl,
%    write('Is human playing X or O (X moves first)? '),
%    read(M),
%    human_playing(M), !
%    .
%

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .

human_playing(M) :- 
    (M == 'x' ; M == 'X'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :- 
    (M == 'o' ; M == 'O'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
    .

human_playing(M) :-
    nl,
    write('Please enter X or O.'),
    set_players(1)
    .


moves(B,L) :-
    not(winner(B,x)),                %%% if either player already won, then there are no available moves
    not(winner(B,o)),
    empty_mark(E),
    findall(X,(board(B),column(B,X,C),square(C,6,S),empty_mark(S)),L).
    L \= []
    .

%.......................................
% utility
%.......................................
% determines the value of a given board position
%

utility(B,U) :-
    winner(B,'x'),
    U = 1,
    !
    .

utility(B,U) :-
    winner(B,'o'),
    U = (-1), 
    !
    .

utility(B,U) :-
    U = 0
    .


%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a tie, so the algorithm is effectively playing not-to-lose.

% For the opening move against an optimal player, the best minimax can ever hope for is a tie.
% So, technically speaking, any opening move is acceptable.
% Save the user the trouble of waiting  for the computer to search the entire minimax tree 
% by simply selecting a random square.

minimax(D,B,M,S,U) :-   
    empty_board(E),
    random_int_1n(6,S),
    !
    .

minimax(D,B,M,S,U) :-
    D2 is D + 1,
    moves(B,L),          %%% get the list of available moves
    !,
    best(D2,B,M,L,S,U),  %%% recursively determine the best available move
    !
    .

% if there are no more available moves, 
% then the minimax value is the utility of the given board position

minimax(D,B,M,S,U) :-
    utility(B,U)      
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list...

best(D,B,M,[S1],S,U) :-
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 
    !,  
    minimax(D,B2,M2,_S,U),  %%% then recursively search for the utility value of that move.
    S = S1, !,
    output_value(D,S,U),
    !
    .

% if there is more than one move in the list...

best(D,B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    minimax(D,B2,M2,_S,U1),      %%% recursively search for the utility value of that move,
    best(D,B,M,T,S2,U2),         %%% determine the best move of the remaining moves,
    output_value(D,S1,U1),      
    better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .


%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(D,M,S1,U1,S2,U2,S,U) :-
    maximizing(M),                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    S = S1,
    U = U1,
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    minimizing(M),                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    S = S1,
    U = U1, 
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    random_int_1n(10,R),               %%% then pick one of them at random
    better2(D,R,M,S1,U1,S2,U2,S,U),    
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-        %%% otherwise, second move is better
    S = S2,
    U = U2,
    !
    .


%.......................................
% better2
%.......................................
% randomly selects two squares of the same utility value given a single probability
%

better2(D,R,M,S1,U1,S2,U2,S,U) :-
    R < 6,
    S = S1,
    U = U1, 
    !
    .

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    S = S2,
    U = U2,
    !
    .





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


print_square(B, IC, IL) :-
	column(B, IC, C),
    square(C, IL, M),
    write_square(C, IL, IC, M)
    .


%prints a line from top
print_line(B, PL) :-
    IL is 7-PL,
	print_square(B, 1, IL),
    write('|'),
	print_square(B, 2, IL),
    write('|'),
	print_square(B, 3, IL),
    write('|'),
	print_square(B, 4, IL),
    write('|'),
	print_square(B, 5, IL),
    write('|'),
	print_square(B, 6, IL),
    write('|'),
	print_square(B, 7, IL)
    .

print_board(B) :-
    print_board(B, 1)
    .

print_board(B, 6) :-
    nl,
    print_line(B, 6)
    .

print_board(B, ITH) :-
    nl,
    print_line(B, ITH),
    nl,
    write("-----------------------------------------"),
    NITH is ITH+1,
    print_board(B,NITH)
    .

write_square(M) :-
    write('  '),
    write(M),
    write('  ')
    .

%if the square contains a empty mark is empty
write_square(M) :-
    empty_mark(M),
    write('  '),
    write(' '),
    write('  ')
    .

%if is playable square
write_square(C,L, IC, M) :-
    playable_square(C,L),
    write('  '),
    write(IC),
    write('  ')
    .

%if the square contains a empty mark is empty
write_square(C, L, IC, M) :-
    empty_mark(M),
    write('  '),
    write(' '),
    write('  ')
    .

write_square(C, L, IC, M) :-
    write('  '),
    write(M),
    write('  ')
    .

output_winner(B) :-
    winner(B,x),
    write('X wins.'),
    !
    .


output_winner(B) :-
    winner(B,o),
    write('O wins.'),
    !
    .

output_winner(B) :-
    write('No winner.')
    .

output_value(D,S,U) :-
    D == 1,
    nl,
    write('Square '),
    write(S),
    write(', utility: '),
    write(U), !
    .

output_value(D,S,U) :- 
    true
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List manipulation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set item in the board by the column
replace_item(T1, TargetCol, V, T2) :-
    set_item2(T1, TargetCol, V, 1, T2).

set_item2([], _, _, _, []) :- !.

set_item2([_|T1], TargetCol, V, CurrentCol, [V|T2]) :-
    CurrentCol = TargetCol,!,
    NewCol is CurrentCol + 1,
    set_item2(T1, -1, V, NewCol, T2).

set_item2([H|T1], TargetCol, V, CurrentCol, [H|T2]) :-
    NewCol is CurrentCol + 1,
    set_item2(T1, TargetCol, V, NewCol, T2).


game_over(P,B) :-
    game_over2(P,B)
    .

game_over2(P, B) :-
    player_mark(P,M),
    winner(B, M)
    .

game_over2(P, B) :-
    column(B,J,C),
    empty_mark(E),
    not(square(C,6,E))    %%% game is over if opponent wins
    .

%.......................................
% random_int_1n
%.......................................
% returns a random integer from 1 to N
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .
