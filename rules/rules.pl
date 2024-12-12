%%% Definitions %%%
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

% Maximum number of items generated in structure
max_items(7).



%%% Rules %%%
% Rule to generate a structure that satisfies the "at least" condition
at_least(Attribute, N, Structure) :-
    max_items(MaxItems),
    random_between(N, MaxItems, TotalItemAmount), % Ensure at least N items in the structure, max 5, TotalItemAmount is the random value
    generate_items_with_attribute(Attribute, N, AttributeItems),
    Remaining is TotalItemAmount - N,
    generate_items(Remaining, RandomItems),
    append(AttributeItems, RandomItems, FullStructure),
    random_permutation(FullStructure, Structure). % Shuffle the structure for randomness

% Rule to generate a structure that satisfies the "exactly" condition
exactly(Attribute, N, Structure) :-
    max_items(MaxItems),
    random_between(N, MaxItems, TotalItemAmount),
    generate_items_with_attribute(Attribute, N, AttributeItems),
    Remaining is TotalItemAmount - N,
    generate_items_without_attribute(Attribute, Remaining, RandomItems),
    append(AttributeItems, RandomItems, FullStructure),
    random_permutation(FullStructure, Structure).

% Rule to generate a structure that satisfies the "more...than" condition
more_than(Attribute1, Attribute2, Structure) :-
    max_items(MaxItems),
    random_between(1, MaxItems, TotalItemAmount),
    Tmp is TotalItemAmount - 1,
    random_between(0, Tmp, RandomItemAmount),
    Remaining is TotalItemAmount - RandomItemAmount,
    generate_random_greater_numbers(Remaining, GreaterN, LowerN),
    generate_items_with_attribute(Attribute1, GreaterN, GreaterItems),
    generate_items_with_attribute(Attribute2, LowerN, LowerItems),
    generate_items_without_attribute(Attribute2, RandomItemAmount, RandomItems),
    append(GreaterItems, LowerItems, TmpStructure),
    append(TmpStructure, RandomItems, FullStructure),
    random_permutation(FullStructure, Structure).

% Rule to generate a structure that satisfies the "odd" condition for the total amount of items in structure
odd_number_of(Structure) :-
    max_items(MaxItems),
    Tmp is MaxItems - 1, % subtract one for not getting out of bounds
    random_between(1, Tmp, TotalItemAmount),
    (TotalItemAmount mod 2 =:= 0 -> OddAmount is TotalItemAmount + 1; OddAmount = TotalItemAmount),
    generate_items(OddAmount, Structure).

% Rule to generate a structure that satisfies the "even" condition for the total amount of items in structure
even_number_of(Structure) :-
    max_items(MaxItems),
    Tmp is MaxItems - 1, % subtract one for not getting out of bounds
    random_between(1, Tmp, TotalItemAmount),
    (TotalItemAmount mod 2 =:= 1 -> EvenAmount is TotalItemAmount + 1; EvenAmount = TotalItemAmount),
    generate_items(EvenAmount, Structure).

% Rule to generate a structure that satisfies the "odd" condition for a specific attribute in the structure
odd_number_of(Attribute, Structure) :-
    max_items(MaxItems),
    random_between(1, MaxItems, TotalItemAmount),
    TmpTotalAmount is TotalItemAmount - 1,
    random_between(0, TmpTotalAmount, TmpAmount),
    (TmpAmount mod 2 =:= 0 -> OddAmount is TmpAmount + 1; OddAmount = TmpAmount),
    generate_items_with_attribute(Attribute, OddAmount, OddItems),
    Remaining is TotalItemAmount - OddAmount,
    generate_items_without_attribute(Attribute, Remaining, RemainingItems),
    append(OddItems, RemainingItems, FullStructure),
    random_permutation(FullStructure, Structure).

% Rule to generate a structure that satisfies the "even" condition for a specific attribute in the structure
even_number_of(Attribute, Structure) :-
    max_items(MaxItems),
    random_between(2, MaxItems, TotalItemAmount),
    TmpTotalAmount is TotalItemAmount - 1,
    random_between(1, TmpTotalAmount, TmpAmount),
    (TmpAmount mod 2 =:= 1 -> EvenAmount is TmpAmount + 1; EvenAmount = TmpAmount),
    generate_items_with_attribute(Attribute, EvenAmount, EvenItems),
    Remaining is TotalItemAmount - EvenAmount,
    generate_items_without_attribute(Attribute, Remaining, RemainingItems),
    append(EvenItems, RemainingItems, FullStructure),
    random_permutation(FullStructure, Structure).

% Rule to generate a structure that satisfies the "either N1 or N2" condition for the total amount of items in structure
either_or(N1, N2, Structure) :-
    max_items(MaxItems),
    random_between(0, 1, RandBit),
    (RandBit = 0 -> RandItemAmount = N1; RandItemAmount = N2),
    (RandItemAmount > MaxItems -> TotalItemAmount = MaxItems; TotalItemAmount = RandItemAmount),
    generate_items(TotalItemAmount, Structure).



%%% Generating %%%
% Generate exactly N items with the specified attribute
generate_items_with_attribute(_, 0, []) :- !. % Base case: no more items to generate
generate_items_with_attribute(Attribute, N, [Item | Rest]) :-
    generate_item_with_attribute(Attribute, Item), % Generate an item with the specified attribute
    N1 is N - 1,
    generate_items_with_attribute(Attribute, N1, Rest).

% Generate an item where one of the attributes is fixed
generate_item_with_attribute(Attribute, item(Color, Shape, Orientation)) :-
    (color(Attribute) ->
        Color = Attribute,
        random_shape(Shape),
        random_orientation(Orientation);
     shape(Attribute) ->
        Shape = Attribute,
        random_color(Color),
        random_orientation(Orientation);
     orientation(Attribute) ->
        Orientation = Attribute,
        random_color(Color),
        random_shape(Shape)
    ).

% Generate random items, ensuring none of them have the specified attribute
generate_items_without_attribute(_, 0, []) :- !. % Base case: no more items to generate
generate_items_without_attribute(Attribute, N, [Item | Rest]) :-
    generate_item_without_attribute(Attribute, Item), % Generate an item without the specified attribute
    N1 is N - 1,
    generate_items_without_attribute(Attribute, N1, Rest).

% Generate an item where no attribute matches the specified one
generate_item_without_attribute(Attribute, item(Color, Shape, Orientation)) :-
    generate_different(color, Attribute, Color),
    generate_different(shape, Attribute, Shape),
    generate_different(orientation, Attribute, Orientation).

% General rule to generate a value different from the given Attribute
generate_different(Type, Attribute, Value) :-
    % Dynamically call the appropriate random selection based on the type
    (Type = color -> random_color_exclude(Attribute, Value); Type = shape -> random_shape_exclude(Attribute, Value); Type = orientation -> random_orientation_exclude(Attribute, Value)),
    Value \= Attribute. % Ensure the generated value is not the same as the Attribute

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

% Random selection of attributes with one excluded attribute
random_color_exclude(Attribute, Color) :-
    findall(C, color(C), PossibleColors),
    exclude(=(Attribute), PossibleColors, FilteredColors),
    random_member(Color, FilteredColors).
random_shape_exclude(Attribute, Shape) :-
    findall(S, shape(S), PossibleShapes),
    exclude(=(Attribute), PossibleShapes, FilteredShapes),
    random_member(Shape, FilteredShapes).
random_orientation_exclude(Attribute, Orientation) :-
    findall(O, orientation(O), PossibleOrientations),
    exclude(=(Attribute), PossibleOrientations, FilteredOrientations),
    random_member(Orientation, FilteredOrientations).

% Generates two random numbers and guarantees that first number is greater than second
generate_random_greater_numbers(TotalAmount, FirstNumber, SecondNumber) :-
    UpperBound is TotalAmount + 1,
    random(1, UpperBound, FirstNumber), % Ensure FirstNumber >= 1
    FirstMinusOne is FirstNumber - 1,
    Remaining is TotalAmount - FirstNumber,
    min_list([FirstMinusOne, Remaining], MaxSecondNumber),
    (
        MaxSecondNumber > 0 -> MaxSecondBound is MaxSecondNumber + 1, random(0, MaxSecondBound, SecondNumber);
        SecondNumber = 0
    ).