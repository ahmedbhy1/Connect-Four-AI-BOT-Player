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

%Writes A square
get_square(B,IC, IL, M) :-
    column(B, IC, C),
    square(C, IL, M).


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

print_square(B, IC, IL) :-
    get_square(B,IC, IL, M),
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



% Set item in the board by the column
% Entry point: set an element `V` in matrix `M` at column `Col`, starting from the lowest row.
set_item_column(M, Col, V, M2) :-
    length(M, RowCount),                % Get the total number of rows in the matrix
    find_empty_cell(M, RowCount, Col, 1, TargetRow), % Find the closest empty cell ('e')
    set_item(M, TargetRow, Col, V, M2). % Place the value in the target row.

% Find the closest empty cell ('e') in column `Col`, starting from the given row `StartRow`.
find_empty_cell([Row|_], StartRow, Col, CurrentRow, CurrentRow) :-
    CurrentRow =< StartRow,             % Ensure we start searching from `StartRow`
    nth1(Col, Row, e).                  % Check if column `Col` in the current row is 'e'.

find_empty_cell([_|T], StartRow, Col, CurrentRow, TargetRow) :-
    CurrentRow < StartRow,              % Skip rows below the starting row
    NextRow is CurrentRow + 1,
    find_empty_cell(T, StartRow, Col, NextRow, TargetRow).

find_empty_cell([_|T], StartRow, Col, CurrentRow, TargetRow) :-
    NextRow is CurrentRow + 1,          % Continue searching in the next row
    find_empty_cell(T, StartRow, Col, NextRow, TargetRow).

% Place a value `V` in matrix `M` at row `X` and column `Y`.
set_item(M, X, Y, V, M2) :-
    set_row(M, X, Y, V, 1, M2).

set_row([], _, _, _, _, []).

set_row([Row|T1], X, Y, V, CurrentRow, [UpdatedRow|T2]) :-
    CurrentRow = X,
    set_item2(Row, Y, V, 1, UpdatedRow), % Replace the element in the target row
    NewRow is CurrentRow + 1,
    set_row(T1, -1, Y, V, NewRow, T2).

set_row([Row|T1], X, Y, V, CurrentRow, [Row|T2]) :-
    NewRow is CurrentRow + 1,
    set_row(T1, X, Y, V, NewRow, T2).

set_item2([], _, _, _, []).

set_item2([_|T1], TargetCol, V, CurrentCol, [V|T2]) :-
    CurrentCol = TargetCol,
    NewCol is CurrentCol + 1,
    set_item2(T1, -1, V, NewCol, T2).

set_item2([H|T1], TargetCol, V, CurrentCol, [H|T2]) :-
    NewCol is CurrentCol + 1,
    set_item2(T1, TargetCol, V, NewCol, T2).

