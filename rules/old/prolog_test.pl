
%%%%%% Facts %%%%%%
shape(pyramid, blue).
shape(pyramid, yellow).
shape(pyramid, red).
shape(block, blue).
shape(block, yellow).
shape(block, red).
shape(wedge, blue).
shape(wedge, yellow).
shape(wedge, red).

%%%%%% Rules %%%%%%
% checks if specific item X is in a list
% [] indicates a list
contains(X, [X|_]).
contains(X, [_|T]) :- contains(X, T).
% checks the length of a given list (recursively)
% given the base case: empty list has length 0
len([], 0). 
len([_|T], L) :- len(T, L1), L is L1 + 1.
% defines maximum structure length
max_len(3).

% correct structure rule (This must be implemented for a specific zendo rule)
% variables start with upper case letter
% "," indicates a conjunction (";" a disjunction)
correct(Structure) :- contains(shape(pyramid, red), Structure), len(Structure, L), max_len(Max), L =< Max.


%%%%%% Example query %%%%%%
% ?- correct([shape(pyramid, red), shape(block, blue)]).


%%%%%% Example findall %%%%%%
% general structure: findall(template, goal, list)
% ?- findall(Color, shape(pyramid, Color), Colors). % finds all 
% ?- findall((Type1, Type2), (shape(Type1, _), shape(Type2, _)), Pairs).
% ?- findall(Structure, correct(Structure), Structures).