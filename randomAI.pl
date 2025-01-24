:- use_module(connect-four, [playable_square/2]).
:- use_module(connect-four, [play/4]).

%Search for available columns to play
available_columns(Board, ColumnsList) :-
    findall(Column, (playable_square(Board, Column)), ColumnsList).


%Choose a random number between te available columns to play
