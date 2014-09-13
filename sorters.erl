-module(sorters).
-author("Jonathan Fragoso").
-vsn(1).
-export([quicksort/1]).
-import(io, [format/1]).

%% Sorts a list of numbers using QuickSort Algorithm
quicksort(NumbersList) -> hello(), 2 + 4.

hello() -> 
	%% io:format/1 is the standard function used to output text.
	format("Hello, world!~n").
	
