-module(max).
-export([my_max/1, pmax/1, send_max/2]).


% SEQUENTIAL MAX

% case the list is empty
my_max([]) -> throw("You cannot find the max of an empty list.");
my_max([H|T]) -> my_max(T, H).

my_max([H|T], Max) when H > Max -> my_max(T, H);
my_max([_|T], Max) 				-> my_max(T, Max);
my_max([],    Max)              -> Max.


% PARALLEL MAX

% case the list is empty
pmax([]) -> 
	throw("You cannot find the max of an empty list.");

pmax(L) when length(L) < 10 -> 
	my_max(L);

pmax(L) ->
	{L1, L2} = halve_list(L),
	P1 = spawn(max, send_max, [self(), L1]),
	P2 = spawn(max, send_max, [self(), L2]),
	Max1 = rcv(P1),
	Max2 = rcv(P2),
	my_max([Max1]++[Max2]).


% HALVE LIST
halve_list(L) ->
	lists:split(length(L) div 2, L).

% receiving max message
rcv(Pid) ->
	receive
		{Pid, Max} -> Max
	end.	

% sending the max
send_max(Pid, L) ->
	Pid ! {self(), my_max(L)}.

% execution sample
% max:my_max([1,4,7,3,10,5,13,4,6,3,2]).
% max:pmax([1,4,7,3,10,5,13,4,6,3,22,33,10,2,3,1,5,32,44,69,3,86,5,4,3,5,7,32,22,25,52,4,6,9,11]).
