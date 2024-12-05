%%%%%% Facts %%%%%%
% Shapes
shape(pyramid).
shape(block).
shape(wedge).

% Colors
color(blue).
color(yellow).
color(red).

% Positions
position(flat).
position(vertical).
position(upright).
position(upside_down).
position(weird).

% Interactions
%interaction(touching).
%interaction(grounded).
%interaction(pointing).
%interaction(on_top_of).

% Item
% One item has a shape, color and position
item(Shape, Color, Position).
% A structure contains multiple items (Example)
% structure(Items).


%%%%%% Rules %%%%%%
% Check if two pieces point in opposite directions
opposite_direction(flat, upright).
opposite_direction(upright, flat).
opposite_direction(upside_down, vertical).
opposite_direction(vertical, upside_down).
opposite_direction(weird, flat). % Example rule, adjust as needed

% Rule for the structure
rule_opposite_directions(Structure) :-
    member(item(_, _, Pos1), Structure),
    member(item(_, _, Pos2), Structure),
    opposite_direction(Pos1, Pos2).



% exactly NUMBER of pieces  of a specific type
exactly(_, [], 0).
exactly(Type, [item(Type, _, _)|T], Number) :- exactly(Type, T, SubNumber), Number is SubNumber + 1.
exactly(Type, [_|T], Number) :- exactly(Type, T, Number).

% Rule: More pyramids than wedges
rule_more_pyramids_than_wedges(Structure) :-
    count_type(pyramid, Structure, NumPyramids),
    count_type(wedge, Structure, NumWedges),
    NumPyramids > NumWedges.

% at least

% exactly

% more than

% odd number

% even number

% either or

% and

% or

% touching

% grounded

% pointing

% on top of

%




%%%%%% Generation %%%%%%
% Test a structure against a rule
satisfies_rule(Rule, Structure) :-
    call(Rule, Structure).

% Generate all valid structures
generate_structures(Rule, ValidStructures) :-
    findall(Structure, (structure(Structure), satisfies_rule(Rule, Structure)), ValidStructures).
