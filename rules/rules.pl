:- use_module(library(apply)).

%%% Facts %%%
orientation(vertical).
orientation(flat).
orientation(upright).
orientation(upside_down).
orientation(cheesecake).
%orientation(weird).

color(blue).
color(yellow).
color(red).

shape(pyramid).
shape(wedge).
shape(block).

interaction(grounded). % specific case: handled like attribute and not an interaction, so check with attribute rules
interaction(touching(_)).
interaction(pointing(_)).
interaction(on_top_of(_)).
interaction(inside(_)).

max_items(2).



%%% Generating %%%
% Generate repeatedly until a structure satisfies all checks (main function)
generate_valid_structure(Checks, Structure) :-
    repeat,
    generate_structure(Structure),
    (and(Checks) -> !; fail).

generate_structure(Structure) :-
    max_items(Max),
    random_between(1, Max, N),
    generate_items(N, N, Structure).

generate_items(0, _, []) :- !.
generate_items(N, TotalN, [item(Id,C,S,O,I)|Rest]) :-
    Id is N - 1,
    random_color(C),
    random_shape(S),
    random_orientation(S, O),
    MaxId is TotalN - 1,
    random_interaction(MaxId, Id, I),
    N1 is N - 1,
    generate_items(N1, TotalN, Rest).

random_color(Color) :-
    findall(C, color(C), Colors),
    random_member(Color, Colors).

random_shape(Shape) :-
    findall(S, shape(S), Shapes),
    random_member(Shape, Shapes).

random_orientation(Shape, Orientation) :-
    %findall(O, orientation(O), Orientations),
    % exclude specific cases for the different shapes
    findall(O, (orientation(O), (
        (Shape = block, O \= cheesecake);
        (Shape = wedge, O \= upside_down);
        (Shape = pyramid, O \= cheesecake, O \= upside_down)
    )), ValidOrientations),
    random_member(Orientation, ValidOrientations).

random_interaction(MaxId, Id, Interaction) :-
    findall(I, interaction(I), Interactions),
    random_member(Template, Interactions),
    assign_random_id(Template, MaxId, Id, Interaction).

assign_random_id(grounded, _, _, grounded).
assign_random_id(touching(_), MaxId, Id, touching(T)) :-
    random_between(0, MaxId, T),
    T \= Id.
assign_random_id(pointing(_), MaxId, Id, pointing(T)) :-
    random_between(0, MaxId, T),
    T \= Id.
assign_random_id(on_top_of(_), MaxId, Id, on_top_of(T)) :-
    random_between(0, MaxId, T),
    T \= Id.
assign_random_id(inside(_), MaxId, Id, inside(T)) :-
    random_between(0, MaxId, T),
    T \= Id.

item_has_attribute(Attr, item(_,C,S,O,I)) :-
    Attr = C; Attr = S; Attr = O; Attr = I.

item_has_two_attributes(A1, A2, Item) :-
    item_has_attribute(A1, Item),
    item_has_attribute(A2, Item).

count_attribute(Attr, Structure, Count) :-
    include(item_has_attribute(Attr), Structure, Filtered),
    length(Filtered, Count).

count_multiple_attributes(A1, A2, Structure, Count) :-
    include(item_has_two_attributes(A1, A2), Structure, Filtered),
    length(Filtered, Count).

% Check if an item has QAttr and an interaction of type InteractionName that leads to another item with IAttr
% #TODO: Only one on-top-of possible for every shape/orientation (only flat block can have two on top)
% #TODO: Vertical/Upright/Upside-down doesn't point on something
% #TODO: Grounded can only point on grounded
% #TODO: Every object can only have one inside. The other need to be on-top-of
% #TODO: Maximum 5 touching items for one target item (4 grounded, one on top)
has_interaction_attribute(QAttr, IAttr, InteractionName, Structure, item(_,C,S,O,I)) :-
    item_has_attribute(QAttr, item(_,C,S,O,I)),
    decode_interaction(I, InteractionName, Target),
    Target \= none, % Only interactions with a target are checked here
    member(item(Target,TC,TS,TO,TI), Structure),
    item_has_attribute(IAttr, item(Target,TC,TS,TO,TI)).

% Decode the interaction to see if it matches the given name and extract the target if any.
decode_interaction(grounded, grounded, none).
decode_interaction(grounded, Name, none) :- Name \= grounded.
decode_interaction(I, Name, T) :-
    I =.. [Name,T], !.
decode_interaction(_, _, none).




%%% Rules %%%
% Check predicates (pure checks, no generation)
at_least(Attr, N, Structure) :-
    count_attribute(Attr, Structure, C),
    C >= N.

at_least(A1, A2, N, Structure) :-
    count_multiple_attributes(A1, A2, Structure, C),
    C >= N.

% Generic version for any interaction that involves a target item
at_least_interaction(QAttr, IAttr, InteractionName, N, Structure) :-
    findall(Item,
        (member(Item, Structure),
        has_interaction_attribute(QAttr, IAttr, InteractionName, Structure, Item)),
        Filtered),
    length(Filtered, Count),
    Count >= N.

exactly(Attr, N, Structure) :-
    count_attribute(Attr, Structure, C),
    C =:= N.

exactly(A1, A2, N, Structure) :-
    count_multiple_attributes(A1, A2, Structure, C),
    C =:= N.

% Generic version for any interaction that involves a target item
exactly_interaction(QAttr, IAttr, InteractionName, N, Structure) :-
    findall(Item,
        (member(Item, Structure),
        has_interaction_attribute(QAttr, IAttr, InteractionName, Structure, Item)),
        Filtered),
    length(Filtered, Count),
    Count =:= N.

more_than(A1, A2, Structure) :-
    count_attribute(A1, Structure, C1),
    count_attribute(A2, Structure, C2),
    C1 > C2.

odd_number_of(Structure) :-
    length(Structure, L),
    1 is L mod 2.

even_number_of(Structure) :-
    length(Structure, L),
    L \= 0,
    0 is L mod 2.

odd_number_of(Attr, Structure) :-
    count_attribute(Attr, Structure, C),
    1 is C mod 2.

even_number_of(Attr, Structure) :-
    count_attribute(Attr, Structure, C),
    C \= 0,
    0 is C mod 2.

odd_number_of_interaction(QAttr, IAttr, InteractionName, Structure) :-
    findall(Item,
        (member(Item, Structure),
        has_interaction_attribute(QAttr, IAttr, InteractionName, Structure, Item)),
        Filtered),
    length(Filtered, C),
    1 is C mod 2.

even_number_of_interaction(QAttr, IAttr, InteractionName, Structure) :-
    findall(Item,
        (member(Item, Structure),
        has_interaction_attribute(QAttr, IAttr, InteractionName, Structure, Item)),
        Filtered),
    length(Filtered, Count),
    Count \= 0,
    0 is Count mod 2.

either_or(N1, N2, Structure) :-
    length(Structure, L),
    (L =:= N1; L =:= N2).

and([]).
and([C|Cs]) :-
    call(C),
    and(Cs).

or([C|_]) :-
    call(C).
or([_|Cs]) :-
    or(Cs).
