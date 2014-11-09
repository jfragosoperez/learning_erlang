-module(poe).
-export([seq_sum_odds_evens/1, par_sum_odds_evens/1, send_sum/2]).


% sum of the numbers in a list.
sum([],N) -> N;
sum([H|T], N) -> sum(T, H+N).
sum(L) -> sum(L, 0).

% splits L into two sub-lists one containing the event numbers,
% the other containing the odd numbers. This function returns
% both lists.
odds_evens(L) -> 
	% list comprehension link:
	% http://learnyousomeerlang.com/starting-out-for-real#list-comprehensions
	% using list comprehension as a way to find the odds from the list
	Odds = [X || X <- L, X rem 2 == 1],
	% using list comprehension as a way to find the evens from the list
	Evens = [X || X <- L, X rem 2 == 0],
	% result: Odds and Evens lists
	{Odds, Evens}.

% sequential version of sum odds and events of a list L.
seq_sum_odds_evens(L) ->
	{Odds, Evens} = odds_evens(L),
	{sum(Odds), sum(Evens)}.	


% parallel version of seq sum odds and events of a list L.
par_sum_odds_evens(L) ->
	{Odds, Evens} = odds_evens(L),
	P1 = spawn(poe, send_sum, [self(), Odds]),
	P2 = spawn(poe, send_sum, [self(), Evens]),
	OddsSum = rcv(P1),
	EvensSum = rcv(P2),
	{OddsSum, EvensSum}.

rcv(Pid) ->
	receive
		{Pid, List} -> List
	end.	

send_sum(P,L) -> 
	P ! {self(), sum(L)}.