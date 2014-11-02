-module(mergesort).
-export([ms/1, pms/1, p_ms/2]).

% SEPARATE INTO TWO LISTS USING N AS THE LENGTH OF THE FIRST LIST,
% AND THUS, THE SECOND LIST WILL HAVE length(L)-N elements

% case we say the length of the list1 is 0
sep(L, 0) -> 
	{[], L};

sep([H|T],N) ->
   {Lleft, Lright} = sep(T,N-1),
   	{append([H],Lleft), Lright}.   

append(L1,L2) ->
	L1++L2.


% MERGE RETURNING SORTED LIST OF TWO LISTS
merge(L1,L2) ->
	merge(L1,L2,[]).

merge([], [], RESULT_LIST) -> RESULT_LIST;
merge([], SECOND_LIST, RESULT_LIST) -> append(RESULT_LIST, SECOND_LIST);
merge(FIRST_LIST, [], RESULT_LIST) -> append(RESULT_LIST, FIRST_LIST);
merge([H1|FIRST_LIST], [H2|SECOND_LIST], RESULT_LIST) ->
	if 	H1 < H2 ->
			merge(FIRST_LIST, [H2|SECOND_LIST], append(RESULT_LIST, [H1]));
		true ->	
			merge([H1|FIRST_LIST], SECOND_LIST, append(RESULT_LIST, [H2]))
	end.	

% MERGE SORT IMPLEMENTATION
ms([]) -> [];
ms([X]) -> [X];
ms(L) ->
   {L1, L2} = sep(L, length(L) div 2),
   merge(ms(L1), ms(L2)).


% PARALLEL VERSION OF MERGE SORT

rcvp(Pid) -> 
	receive
		{Pid, L} -> L
	end.

pms(L) ->
	Pid = spawn(msort, p_ms, [self(), L]),
    rcvp(Pid).

p_ms(Pid, L) when length(L) < 100 -> Pid ! {self(), ms(L)}; %Do mergesort
p_ms(Pid, L) -> %Else, if the length is "long", spawn processes
	{Lleft, Lright} = sep(L, length(L) div 2),
    Pid1 = spawn(msort, p_ms, [self(), Lleft]),
    Pid2 = spawn(msort, p_ms, [self(), Lright]),
    L1 = rcvp(Pid1),
    L2 = rcvp(Pid2),
    Pid ! {self(), merge(L1,L2)}. %Send to father the merged lists
    
