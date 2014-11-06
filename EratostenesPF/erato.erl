-module(erato). 
-export([erato/1]). 
-export([filter/1, generator/2]). %reason: to spawn


%Filter_pipeline
%We have a Generator of numbers(GEN(N)) connected to the mailbox of the first filter.
%1,2,...,N,eos (end of sequence)
%Filter1:
    %The first number that arrives is set as a prime (2)
    %This filter removes all the multiples of 2 and send the rest to Filter 2.

%Filter2:
    %Prime(3)... etc etc.

% N >= 2 
erato(N) -> FirstFilterPid = spawn(erato, filter, [[]]), 
            spawn(erato, generator, [N, FirstFilterPid]),      
            ok.

% N >= 2
generator(N, Pid) -> generator(2, N, Pid). %First message of the generator.

%N >= 2, K <= N + 1 
generator(K, N, Pid) when K == N + 1 -> Pid ! eos; 
generator(K, N, Pid) -> Pid ! K, 
                        generator(K+1,N,Pid).

% Primes is the list of primes already generated 
filter(Primes) -> 
    receive 
        eos -> 
            io:format("~p~n",[lists:reverse(Primes)]); 
        P -> 
            NextFilterPid = spawn(erato, filter, [[P|Primes]]), 
            loop(P, NextFilterPid)
    end.

loop(P, NextFilterPid) -> 
    receive 
        eos -> 
            NextFilterPid ! eos;
        M when M rem P /= 0 -> 
            NextFilterPid ! M,     %send it to the next filter
            loop(P,NextFilterPid); %loop again
        M when M rem P == 0 -> 
            loop(P,NextFilterPid) %just loop again 
    end.            

