% Define the possible attributes for items
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

% Define a structure as a list of items
% An item has the form item(Color, Shape, Orientation)

% Rule to check if there are at least N items with a given attribute
% The attribute can be color, shape, or orientation
at_least(Attribute, N, Structure) :-
    generate_structure(Structure),
    count_attribute(Attribute, Structure, Count),
    Count >= N,
    !. % Cut to stop further backtracking

% Generate a random structure
% Adjust the number of items in the structure as needed
generate_structure(Structure) :-
    random_between(2, 5, NumItems), % Generate a random number of items (3 to 6 for this example)
    generate_items(NumItems, Structure).

% Generate a list of random items
generate_items(0, []) :- !. % Base case: no more items to generate
generate_items(N, [item(Color, Shape, Orientation) | Rest]) :-
    random_color(Color),
    random_shape(Shape),
    random_orientation(Orientation),
    N1 is N - 1,
    generate_items(N1, Rest).

% Random selection of attributes
random_color(Color) :- findall(C, color(C), Colors), random_member(Color, Colors).
random_shape(Shape) :- findall(S, shape(S), Shapes), random_member(Shape, Shapes).
random_orientation(Orientation) :- findall(O, orientation(O), Orientations), random_member(Orientation, Orientations).

% Helper rule to count the occurrences of an attribute in the structure
count_attribute(_, [], 0). % Base case: empty structure
count_attribute(Attribute, [item(Attribute, _, _) | Rest], Count) :-
    count_attribute(Attribute, Rest, RestCount),
    Count is RestCount + 1.
count_attribute(Attribute, [item(_, Attribute, _) | Rest], Count) :-
    count_attribute(Attribute, Rest, RestCount),
    Count is RestCount + 1.
count_attribute(Attribute, [item(_, _, Attribute) | Rest], Count) :-
    count_attribute(Attribute, Rest, RestCount),
    Count is RestCount + 1.
count_attribute(Attribute, [item(_, _, _) | Rest], Count) :-
    count_attribute(Attribute, Rest, Count). % Skip item if no match
