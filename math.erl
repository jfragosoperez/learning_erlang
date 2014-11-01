-module(math).
-export([areas/1, area/1, factorial/1, member/2]).
-import(lists, [map/2]).


% sums area using different kind of area function declarations
% example -> math:areas([{rectangle,12,4},{square,6}]).
areas(L) ->
	lists:sum(
		map(
			% Funs erland data structure: "Funs are function closures, called lambda in other languages"
			fun(I) -> area(I) end,
			L)).

% square area calculation
area({square, X}) -> 
	X*X;

% rectangle area calculation
area({rectangle,X,Y}) -> 
	X*Y.

% factorial function
factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

% finding an element in a list
% case that H matches the header of the list
member(H, [H1|_]) when H == H1 -> true;
% if H not matched the header of the list, we call member function 
% with the tail (the other elements of the list)
member(H, [_|T])  -> member(H, T);
% in case the list is empty, we know that H is not found	
member(H, [])    -> false.


% EXECUTION SAMPLE:
% 1> math:area({square,3}).
% 9
% 2> math:area({rectangle,3,4}).
% 12
% 3> math:areas([{rectangle,2,2},{square,2}]).
% 8
% 4> math:factorial(3).
% 6
% 5> math:member("cat", ["dog","lion","snake","cat","frog"]).
% true
% 6> math:member("leopard", ["dog","lion","snake","cat","frog"]).
% false
