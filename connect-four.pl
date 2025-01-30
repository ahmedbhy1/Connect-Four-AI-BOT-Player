%Test fact
player(1, human).
player(2, human).

board([['o','x','e','e','e','e'],
       ['o','x','x','x','e','e'],
       ['e','e','e','e','e','e'],
       ['x','e','e','e','e','e'],
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

player_mark(1, 'x').
player_mark(2, 'o').

next_player(1,2).
next_player(2,1).

%Test fact
board([['o','x','e','e','e','e'],
       ['o','x','x','x','e','e'],
       ['e','e','e','e','e','e'],
       ['x','e','e','e','e','e'],
       ['x','x','o','x','o','o'],
       ['o','o','x','e','e','e'],
       ['o','x','x','o','e','e']]).
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

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

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
    make_move(human, P, B, B2),
    (   game_over(P, B2) ->
        write("game over Player: "),
        write(P),
        write(" win!")
    ;   
        next_player(P, P2),
        play(P2, B2)
    ).

make_move(human, P, B, B2) :-
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
    
make_move(human, P, B, B2) :-
    nl,
    nl,
    write('Please select a numbered column.'),
    make_move(human,P,B,B2)
    .

make_move(computer, P, B, B2) :-
    random_column(B, CN),
    empty_mark(E),
    player_mark(P, M),
    move(B,CN, M, B2), !
    .  

read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Random AI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Search for available columns to play
playable_column(Board, Column) :-
    column(Board, Column, Col),
    playable_square(Col, _).
    

available_columns(Board, ColumnsList) :-
    findall(Column, (playable_column(Board, Column)), ColumnsList).


%Choose a random number between te available columns to play
random_column(Board, Column) :-
    available_columns(Board, ColumnsList),
    length(ColumnsList, Length),
    random(0, Length, Index),
    nth0(Index, ColumnsList, Column)
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
