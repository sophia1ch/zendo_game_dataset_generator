% Define the attributes
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

% Define the rule for "at least"
at_least(Value, MinCount, Structure, Count) :-
    number(MinCount),
    count_attribute(Value, Structure, Count),
    Count >= MinCount.

% Helper predicate to count occurrences of a specific value in a structure
count_attribute(_, [], 0).
count_attribute(Value, [Item|Rest], Count) :-
    item_has_value(Item, Value),
    count_attribute(Value, Rest, RestCount),
    Count is RestCount + 1.
count_attribute(Value, [_|Rest], Count) :-
    count_attribute(Value, Rest, Count).

% Helper predicate to check if an item has a specific value
item_has_value(item(Color, _, _), Color).
item_has_value(item(_, Shape, _), Shape).
item_has_value(item(_, _, Orientation), Orientation).
