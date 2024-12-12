:- use_module(library(apply)). % for include/3

orientation(flat).
orientation(vertical).
orientation(upright).
orientation(upside_down).
orientation(weird).

color(blue).
color(yellow).
color(red).

shape(pyramid).
shape(wedge).
shape(block).

max_items(7).

% Generate a random structure
generate_structure(Structure) :-
    max_items(Max),
    random_between(1, Max, N),
    generate_items(N, Structure).

generate_items(0, []) :- !.
generate_items(N, [item(Color, Shape, Orientation)|Rest]) :-
    random_color(Color),
    random_shape(Shape),
    random_orientation(Orientation),
    N1 is N - 1,
    generate_items(N1, Rest).

random_color(Color) :-
    findall(C, color(C), Colors),
    random_member(Color, Colors).

random_shape(Shape) :-
    findall(S, shape(S), Shapes),
    random_member(Shape, Shapes).

random_orientation(Orientation) :-
    findall(O, orientation(O), Orientations),
    random_member(Orientation, Orientations).

% Counting attributes
count_attribute(Attr, Structure, Count) :-
    include(has_attribute(Attr), Structure, Filtered),
    length(Filtered, Count).

has_attribute(Attr, item(Attr,_,_)).
has_attribute(Attr, item(_,Attr,_)).
has_attribute(Attr, item(_,_,Attr)).

% Check predicates (pure checks, no generation)
check_at_least(Attr, N, Structure) :-
    count_attribute(Attr, Structure, Count),
    Count >= N.

check_exactly(Attr, N, Structure) :-
    count_attribute(Attr, Structure, Count),
    Count =:= N.

check_more_than(A1, A2, Structure) :-
    count_attribute(A1, Structure, C1),
    count_attribute(A2, Structure, C2),
    C1 > C2.

check_odd_number_of(Structure) :-
    length(Structure, L),
    1 is L mod 2.

check_even_number_of(Structure) :-
    length(Structure, L),
    0 is L mod 2.

check_odd_number_of(Attr, Structure) :-
    count_attribute(Attr, Structure, Count),
    1 is Count mod 2.

check_even_number_of(Attr, Structure) :-
    count_attribute(Attr, Structure, Count),
    0 is Count mod 2.

check_either_or(N1, N2, Structure) :-
    length(Structure, L),
    (L =:= N1; L =:= N2).

% Logical combination of checks
and_checks([]).
and_checks([Check|Cs]) :-
    call(Check),
    and_checks(Cs).

or_checks([Check|_]) :-
    call(Check).
or_checks([_|Cs]) :-
    or_checks(Cs).

% Generate repeatedly until a structure satisfies all checks
generate_valid_structure(Checks, Structure) :-
    repeat,
    generate_structure(Structure),
    (and_checks(Checks) -> !; fail).
