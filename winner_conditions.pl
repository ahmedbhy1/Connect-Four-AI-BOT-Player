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
