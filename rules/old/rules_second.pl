% Prolog file to represent Zendo rules and generate structures accordingly.

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

zendo_number(0).
zendo_number(1).
zendo_number(2).
zendo_number(3).

% Define rule parsing and structure generation
apply_rule(Rule, Structure) :-
    parse_rule(Rule, ParsedRule),
    generate_structure(ParsedRule, Structure).

% Parse the rule input into a structured format
parse_rule(Rule, parsed_rule(Condition, Quantity, Attributes)) :-
    split_string(Rule, " ", "", Parts),
    parse_condition(Parts, Condition, Rest),
    parse_quantity(Rest, Quantity, AttrParts),
    parse_attributes(AttrParts, Attributes).

parse_condition(["at", "least" | Rest], at_least, Rest).
parse_condition(["exactly" | Rest], exactly, Rest).
parse_condition(["more" | Rest], more_than, Rest).
parse_condition(["an", "odd", "number", "of" | Rest], odd, Rest).
parse_condition(["an", "even", "number", "of" | Rest], even, Rest).
parse_condition(["either" | Rest], either, Rest).

parse_quantity([NumberStr | Rest], quantity(Number), Rest) :-
    atom_number(NumberStr, Number),
    zendo_number(Number).

parse_attributes(["pieces" | Rest], Attributes) :-
    parse_attributes(Rest, Attributes).
parse_attributes([AttributeStr | Rest], [Attribute | Attributes]) :-
    parse_attribute(AttributeStr, Attribute),
    parse_attributes(Rest, Attributes).
parse_attributes([], []).

parse_attribute(AttributeStr, color(AttributeStr)) :-
    color(AttributeStr).
parse_attribute(AttributeStr, shape(AttributeStr)) :-
    shape(AttributeStr).
parse_attribute(AttributeStr, orientation(AttributeStr)) :-
    orientation(AttributeStr).

% Generate a structure that fulfills the parsed rule
generate_structure(parsed_rule(at_least, quantity(N), Attributes), Structure) :-
    findall(Item, generate_item(Attributes, Item), AllItems),
    length(AllItems, TotalItems),
    TotalItems >= N,
    % Select N items from AllItems to form the Structure
    length(Structure, N),
    subset(Structure, AllItems).

generate_structure(parsed_rule(exactly, quantity(N), Attributes), Structure) :-
    findall(Item, (generate_item(Attributes, Item)), Items),
    length(Items, N),
    Structure = Items.

generate_structure(parsed_rule(more_than, quantity(N), Attributes), Structure) :-
    findall(Item, (generate_item(Attributes, Item)), Items),
    length(Items, L),
    L > N,
    Structure = Items.

generate_structure(parsed_rule(odd, quantity(_), Attributes), Structure) :-
    findall(Item, (generate_item(Attributes, Item)), Items),
    length(Items, L),
    1 is mod(L, 2),
    Structure = Items.

generate_structure(parsed_rule(even, quantity(_), Attributes), Structure) :-
    findall(Item, (generate_item(Attributes, Item)), Items),
    length(Items, L),
    0 is mod(L, 2),
    Structure = Items.

generate_structure(parsed_rule(either, quantity(N1, N2), Attributes), Structure) :-
    findall(Item, (generate_item(Attributes, Item)), Items),
    length(Items, L),
    (L =:= N1; L =:= N2),
    Structure = Items.

% Generate an item with the given attributes
generate_item(Attributes, item(Orientation, Color, Shape)) :-
    (member(orientation(Orientation), Attributes) ; orientation(Orientation)),
    (member(color(Color), Attributes) ; color(Color)),
    (member(shape(Shape), Attributes) ; shape(Shape)).

% Define subset predicate if not available
subset([], []).
subset([E|Tail], [E|NTail]) :-
    subset(Tail, NTail).
subset(Tail, [_|NTail]) :-
    subset(Tail, NTail).