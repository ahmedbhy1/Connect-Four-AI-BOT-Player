%The utility points for each position in a board
utilityboard([[3,4,5,5,4,3],
       [4,6,7,7,6,4],
       [5,8,11,11,8,5],
       [7,10,13,13,10,7],
       [5,8,11,11,8,5],
       [4,6,7,7,6,4],
       [3,4,5,5,4,3]]).

%Test fact
board([['o','x','e','e','e','e'],
       ['o','x','x','x','e','e'],
       ['e','e','e','e','e','e'],
       ['x','e','e','e','e','e'],
       ['x','x','o','x','o','o'],
       ['o','o','x','e','e','e'],
       ['o','x','x','o','e','e']]).
%---------------

%multiply 2 matrix
m_mult_matr(M1, M2, M3) :- maplist(maplist(multiply), M1, M2, M3).
multiply(X,Y,Z) :- Z is X*Y.

%transform board into matrix of 0s and +-1s
map_char('o', 1).
map_char('x', -1).
map_char('e', 0).

transform_row([],[]).
transform_row([Char|Rest],[Value|TransformedRest]):-
    map_char(Char,Value),
    transform_row(Rest, TransformedRest).

transform_matrix([],[]).
transform_matrix([Row|RestRows],[TransformedRow|TransformedRestRows]):-
    transform_row(Row,TransformedRow),
    transform_matrix(RestRows, TransformedRestRows).

%sum of all elements in the matrix
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

flatten_matrix([],[]).
flatten_matrix([Row|RestRows], Flattened) :-
   flatten_matrix(RestRows, FlattenedRest),
   append(Row, FlattenedRest, Flattened).

matrix_sum(Matrix, Sum) :-
    flatten_matrix(Matrix, Flattened),
    sum_list(Flattened, Sum).

%computes the utility S for a specific board formation B
utilitytest(B,S) :-
    utilityboard(UB),
    transform_matrix(B, B1), 
    m_mult_matr(UB,B1, Res), 
    matrix_sum(Res, S).