% Each array on the matrix is a column of the board from bottom to top.
empty_mark('e').
empty_board([['e','e','e','e','e','e'], ['e','e','e','e','e','e'], ['e','e','e','e','e','e'],
             ['e','e','e','e','e','e'], ['e','e','e','e','e','e'], ['e','e','e','e','e','e'],
             ['e','e','e','e','e','e']]).

%Test fact
board([[1,1,'e','e','e','e'],
       [2,2,2,2,'e','e'],
       ['e','e','e','e','e','e'],
       [4,'e','e','e','e','e'],
       [5,5,5,5,5,5],
       [6,6,6,'e','e','e'],
       [7,7,7,7,'e','e']]).
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
    findall(N, square(C, N, 'e'), L),
    min_list(L,N)
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%writes a square
print_square(B, IC, IL) :-
	column(B, IC, C),
    square(C, IL, M),
    write_square(M)
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
    
print_board :-
    board(B),
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

%if the square contains a empty mark is empty
write_square(M) :-
    empty_mark(M),
    write('  '),
    write(' '),
    write('  ')
    .

write_square(M) :-
    write('  '),
    write(M),
    write('  ')
    .


    
