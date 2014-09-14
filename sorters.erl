-module(sorters).
-author("Jonathan Fragoso").
-vsn(1).
-export([sort/2]).
-import(io, [format/2]).

%% Sorts a list of numbers using QuickSort Algorithm
sort(quicksort, [NumbersList]) -> print_sorting_with("quicksort");
sort(mergesort, [NumbersList]) -> print_sorting_with("mergesort");
sort(insertionsort, [NumbersList]) -> print_sorting_with("insertionsort");
sort(_, [NumbersList]) -> print_sorting_with("").

print_sorting_with(SortAlgorithmName) -> 
	%% io:format/1 is the standard function used to output text.
	format("Sorting list of numbers, using ~s ~n", [SortAlgorithmName]).
	
