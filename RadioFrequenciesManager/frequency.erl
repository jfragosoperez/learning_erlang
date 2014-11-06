-module(frequency). 
-export([start/0, stop/0, allocate/0, deallocate/1]). 
-export([init/0]).

%% These are the start functions used to create and 
%% initialize the server.

start() -> 
    register(frequency , spawn(frequency, init, [])).  %Register frequency as name of the server process

init() ->
    %%Frequencies is a tuple of two lists 
    %%Fist list contains free frequencies 
    %%Second list contains tuples of {busy frecuency, user Pid} 
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Hard Coded frequences
get_frequencies() -> [10,11,12,13,14,15].


%% The client Functions 
allocate() -> call(allocate).

deallocate(Freq) -> call({deallocate, Freq}). 

stop()-> call(stop).

%% We hide all message passing and the message %% protocol in a functional interface.
call(Message) ->
     frequency ! {request, self(), Message}, %Message to the server process :)
     receive 
        {reply, Reply} -> Reply
    end.

%% The Main Loop 
loop(Frequencies) -> 
    receive
        {request, Pid, allocate} -> 
            {NewFrequencies, Reply} = allocate(Frequencies , Pid), 
            reply(Pid, Reply), 
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} -> 
            NewFrequencies = deallocate(Frequencies, Freq ),
            reply(Pid, ok),
            loop(NewFrequencies);
        {request, Pid, stop} -> 
            reply(Pid, ok)
end.

reply(Pid, Reply) -> 
    Pid ! {reply, Reply}.


%% The Internal Help Functions used to allocate and 
%% deallocate frequencies.
allocate({[], Allocated}, _Pid) -> 
    {{[], Allocated}, {error, no_frequency}};

allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) -> 
    NewAllocated=lists:keydelete(Freq, 1, Allocated), %Delete form Allocated
    {[Freq|Free], NewAllocated}. %Add to Free

