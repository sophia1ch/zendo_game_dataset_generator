:- use_module(library(apply)). % for include/3

%%% Facts %%%
orientation(flat).
orientation(vertical).
orientation(upright).
orientation(weird).
%orientation(upside_down).
%orientation(cheesecake).

color(blue).
color(yellow).
color(red).

shape(pyramid).
shape(wedge).
shape(block).

interaction(grounded).
interaction(touching(_)). % "_" is an Id e [0, N-1]
interaction(pointing(_)).
interaction(on_top_of(_)).
interaction(inside(_)).

max_items(3).



%%% Generating %%%
% Generate repeatedly until a structure satisfies all checks (main function)
generate_valid_structure(Checks, Structure) :-
    repeat,
    generate_structure(Structure),
    (and(Checks) -> !; fail).

% Generate a random structure
generate_structure(Structure) :-
    max_items(Max),
    random_between(1, Max, N),
    generate_items(N, N, Structure).

generate_items(0, _, []) :- !.
generate_items(N, TotalN, [item(Id, Color, Shape, Orientation, Interaction)|Rest]) :-
    Id is N - 1,            % set an id for every item, beginning with zero
    random_color(Color),
    random_shape(Shape),
    random_orientation(Orientation),
    MaxId is TotalN - 1,
    random_interaction(MaxId, Id, Interaction),
    N1 is N - 1,
    generate_items(N1, TotalN, Rest).

random_color(Color) :-
    findall(C, color(C), Colors),
    random_member(Color, Colors).

random_shape(Shape) :-
    findall(S, shape(S), Shapes),
    random_member(Shape, Shapes).

random_orientation(Orientation) :-
    findall(O, orientation(O), Orientations),
    random_member(Orientation, Orientations).

random_interaction(MaxId, Id, Interaction) :-
    findall(I, interaction(I), Interactions),
    random_member(InteractionTemplate, Interactions),
    assign_random_id(InteractionTemplate, MaxId, Id, Interaction).

assign_random_id(grounded, _, _, grounded).
assign_random_id(touching(_), MaxId, Id, touching(RandomId)) :-
    random_between(0, MaxId, RandomId),
    RandomId \= Id.
assign_random_id(pointing(_), MaxId, Id, pointing(RandomId)) :-
    random_between(0, MaxId, RandomId),
    RandomId \= Id.
assign_random_id(on_top_of(_), MaxId, Id, on_top_of(RandomId)) :-
    random_between(0, MaxId, RandomId),
    RandomId \= Id.


% Counting attributes
count_attribute(Attr, Structure, Count) :-
    include(has_attribute(Attr), Structure, Filtered),
    length(Filtered, Count).

% Count items with multiple attributes
count_multiple_attributes(Attr1, Attr2, Structure, Count) :-
    include(has_attributes(Attr1, Attr2), Structure, Filtered),
    length(Filtered, Count).

has_attribute(Attr, item(_,Attr,_,_,_)).
has_attribute(Attr, item(_,_,Attr,_,_)).
has_attribute(Attr, item(_,_,_,Attr,_)).
has_attribute(Attr, item(_,_,_,_,Attr)).

has_attributes(Attr1, Attr2, item(_,Attr1,Attr2,_,_)).
has_attributes(Attr1, Attr2, item(_,Attr1,_,Attr2,_)).
has_attributes(Attr1, Attr2, item(_,Attr1,_,_,Attr2)).
has_attributes(Attr1, Attr2, item(_,Attr2,Attr1,_,_)).
has_attributes(Attr1, Attr2, item(_,_,Attr1,Attr2,_)).
has_attributes(Attr1, Attr2, item(_,_,Attr1,_,Attr2)).
has_attributes(Attr1, Attr2, item(_,_,_,Attr1,Attr2)).
has_attributes(Attr1, Attr2, item(_,_,Attr2,Attr1,_)).
has_attributes(Attr1, Attr2, item(_,Attr2,_,Attr1,_)).
has_attributes(Attr1, Attr2, item(_,Attr2,_,_,Attr1)).
has_attributes(Attr1, Attr2, item(_,_,Attr2,_,Attr1)).
has_attributes(Attr1, Attr2, item(_,_,_,Attr2,Attr1)).



%%% Rules %%%
% Check predicates (pure checks, no generation)
at_least(Attr, N, Structure) :-
    count_attribute(Attr, Structure, Count),
    Count >= N.
% Rules like: "... contains at least one green pyramid."
at_least(Attr1, Attr2, N, Structure) :-
    count_multiple_attributes(Attr1, Attr2, Structure, Count),
    Count >= N.

% Interaction combination of at_least
at_least_touching(QAttr, IAttr, N, Structure) :-
    findall(Item,
        (member(Item, Structure),has_touching_attribute(QAttr, IAttr, Structure, Item)),
        Filtered),
    length(Filtered, Count),
    Count >= N.

has_touching_attribute(QAttr, IAttr, Structure, item(_,C,S,O,touching(T))) :-
    has_attribute(QAttr, item(_,C,S,O,_)),
    member(item(T,TC,TS,TO,_), Structure),
    has_attribute(IAttr, item(T,TC,TS,TO,_)).


exactly(Attr, N, Structure) :-
    count_attribute(Attr, Structure, Count),
    Count =:= N.
exactly(Attr1, Attr2, N, Structure) :-
    count_multiple_attributes(Attr1, Attr2, Structure, Count),
    Count =:= N.

more_than(A1, A2, Structure) :-
    count_attribute(A1, Structure, C1),
    count_attribute(A2, Structure, C2),
    C1 > C2.

% Odd number of total pieces
odd_number_of(Structure) :-
    length(Structure, L),
    1 is L mod 2.

% Even number of total pieces
even_number_of(Structure) :-
    length(Structure, L),
    0 is L mod 2.

% Odd number of specific pieces
odd_number_of(Attr, Structure) :-
    count_attribute(Attr, Structure, Count),
    1 is Count mod 2.

% Even number of specific pieces
even_number_of(Attr, Structure) :-
    count_attribute(Attr, Structure, Count),
    0 is Count mod 2.

either_or(N1, N2, Structure) :-
    length(Structure, L),
    (L =:= N1; L =:= N2).


% Logical combination of checks
and([]).
and([Check|Cs]) :-
    call(Check),
    and(Cs).

or([Check|_]) :-
    call(Check).
or([_|Cs]) :-
    or(Cs).

