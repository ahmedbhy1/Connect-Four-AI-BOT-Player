%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% On verifie si un joueur a gagné la partie %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(winner, [winner/2]).

% TO TEST THIS FILE: 
/*
trace, (winner([[z,_,z,z,_,_], 
             	[z,z,_,z,_,_], 
            	[z,_,z,_,_,_],
             	[_,z,_,_,_,_],
             	[z,_,_,_,_,_], 
             	[_,_,_,_,_,_],
             	[_,_,_,_,_,_]], player1)).
*/

% Les positions gangnats sur:
%       - Une collone
winner(Board, Player) :-
    winingInAColumn(Board, Player),!.

%       - Une ligne
winner(Board, Player) :-
    winingInARow(Board, Player),!.

%       - Une diagonal descendant
winner(Board, Player) :-
    winingInADiagonalDesc(Board, Player),!.

%       - Une diagonal ascendant
winner(Board, Player) :-
    winingInADiagonalAsc(Board, Player),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% On verifie si un joueur a gagné la partie sur %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%       - Une collone
winingInAColumn([Column|_], Player) :-
    winingColumn(Column, Player),!.
winingInAColumn([_|Rest], Player) :-
    winingInAColumn(Rest, Player).

%       - Une ligne
winingInARow([[A|_], [B|_], [C|_], [D|_], [E|_], [F|_], [G|_]], Player) :-
    winingRow([A,B,C,D,E,F,G], Player),!.
winingInARow([_|A1], [_,B1], [_,C1], [_,D1], [_,E1], [_,F1], [_,G1], Player) :-
    winingInARow([A1, B1, C1, D1, E1, F1, G1], Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% On define les positions gangnats pour: %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%       - Une collone:

winingColumn([I,II,III,IV,_,_], Player) :-
    I==II, II==III, III==IV, nonvar(Player).
winingColumn([_,II,III,IV,V,_], Player) :-
    II==III, III==IV, IV==V, nonvar(Player).
winingColumn([_,_,III,IV,V,VI], Player) :-
    III==IV, IV==V, V==VI, nonvar(Player).

%       - Une ligne:
winingRow([I,II,III,IV,_,_,_], Player) :-
    I==II, II==III, III==IV, nonvar(Player),!.
winingRow([_,II,III,IV,V,_,_], Player) :-
    II==III, III==IV, IV==V, nonvar(Player),!.
winingRow([_,_,III,IV,V,VI,_], Player) :-
    III==IV, IV==V, V==VI, nonvar(Player),!.
winingRow([_,_,_,IV,V,VI,VII], Player) :-
    IV==V, V==VI, VI==VII, nonvar(Player),!.

%       - Une diagonal descendant:
winingInADiagonalDesc([[I,_,_,_,_,_],[_,II,_,_,_,_],[_,_,III,_,_,_],[_,_,_,IV,_,_],_,_,_], Player) :-
    I==II, II==III, III==IV, nonvar(Player),!.
winingInADiagonalDesc([[_,II,_,_,_,_],[_,_,III,_,_,_],[_,_,_,IV,_,_],[_,_,_,_,V,_],_,_,_], Player) :-
    II==III, III==IV, IV==V, nonvar(Player),!.
winingInADiagonalDesc([[_,_,III,_,_,_],[_,_,_,IV,_,_],[_,_,_,_,V,_],[_,_,_,_,_,VI],_,_,_], Player) :-
    III==IV, IV==V, V==VI , nonvar(Player),!.

winingInADiagonalDesc([_,[I,_,_,_,_,_],[_,II,_,_,_,_],[_,_,III,_,_,_],[_,_,_,IV,_,_],_,_], Player) :-
    I==II, II==III, III==IV, nonvar(Player),!.
winingInADiagonalDesc([_,[_,II,_,_,_,_],[_,_,III,_,_,_],[_,_,_,IV,_,_],[_,_,_,_,V,_],_,_], Player) :-
    II==III, III==IV, IV==V, nonvar(Player),!.
winingInADiagonalDesc([_,[_,_,III,_,_,_],[_,_,_,IV,_,_],[_,_,_,_,V,_],[_,_,_,_,_,VI],_,_], Player) :-
    III==IV, IV==V, V==VI , nonvar(Player),!.

winingInADiagonalDesc([_,_,[I,_,_,_,_,_],[_,II,_,_,_,_],[_,_,III,_,_,_],[_,_,_,IV,_,_],_], Player) :-
    I==II, II==III, III==IV, nonvar(Player),!.
winingInADiagonalDesc([_,_,[_,II,_,_,_,_],[_,_,III,_,_,_],[_,_,_,IV,_,_],[_,_,_,_,V,_],_], Player) :-
    II==III, III==IV, IV==V, nonvar(Player),!.
winingInADiagonalDesc([_,_,[_,_,III,_,_,_],[_,_,_,IV,_,_],[_,_,_,_,V,_],[_,_,_,_,_,VI],_], Player) :-
    III==IV, IV==V, V==VI , nonvar(Player),!.

winingInADiagonalDesc([_,_,_,[I,_,_,_,_,_],[_,II,_,_,_,_],[_,_,III,_,_,_],[_,_,_,IV,_,_]], Player) :-
    I==II, II==III, III==IV, nonvar(Player),!.
winingInADiagonalDesc([_,_,_,[_,II,_,_,_,_],[_,_,III,_,_,_],[_,_,_,IV,_,_],[_,_,_,_,V,_]], Player) :-
    II==III, III==IV, IV==V, nonvar(Player),!.
winingInADiagonalDesc([_,_,_,[_,_,III,_,_,_],[_,_,_,IV,_,_],[_,_,_,_,V,_],[_,_,_,_,_,VI]], Player) :-
    III==IV, III==V, V==VI , nonvar(Player),!.

%       - Une diagonal ascendant:
winingInADiagonalAsc(Board,Winner) :-
    reverse(Board, InversedBoard), winingInADiagonalDesc(InversedBoard, Winner).