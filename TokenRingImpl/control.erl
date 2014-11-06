-module(control).
-export([go/2, worker_loop/1, init_control/2]).

% not required, because we could send only the token atom, 
% but in practice, a token normally has a value.
-define(TOKEN, "fdsjflsakjijnmk").


% setting up a ring of N processes (workers) and sends 
% a token to the first worker.
go(NumberOfWorkers, NumberOfMeals) ->
	ListToConsume = generate_random_int_list(NumberOfMeals),
		io:format("~p~n",[ListToConsume]),
		% creating the ring (which returns the Worker Pids)
		% and sending the token to the first PID
		WorkersPidList = create_worker_ring([], NumberOfWorkers),
		% registering the control process, creating a new control process
		% with the list of pids of the worker processes and the
		% list of data to consume as well.
		register(control_process, spawn(control, init_control, [WorkersPidList, ListToConsume])).

% generates a list L of M random integer numbers in the set {1,2,...,M} 
generate_random_int_list(IntegerRandomListSize) ->
	[random:uniform(IntegerRandomListSize) || _ <- lists:seq(1, IntegerRandomListSize)].		

% empty ring, notify about it.
init_control([], _) ->
	io:format("You cannot create an empty ring~n");

% single person ring, we need this definition because if the
% ring is formed by a single worker, the other method definition
% we can find below, does not work.
% In this case we need to set the successor of the worker to himself. 
init_control([H1|[]], ListToConsume) ->
	set_successors([H1]++[H1]),
	send_token(H1, ?TOKEN),
	control_loop([H1]++[H1], ListToConsume, []);

% initialization of control process. We need the whole list of Pids
% in order to be able to stop all the terminate the workers, when the list
% of objects to consume is empty.
init_control([H1|[H2|WorkersPidList]], ListToConsume)->
	% set first successor and append it to the end (so last element know his successor)
	set_successors([H1|[H2|WorkersPidList++[H1]]]),
	send_token(H1, ?TOKEN),
	control_loop([H1|[H2|WorkersPidList]], ListToConsume, []).

% stopping the recursion of setting the worker successor
% of each worker
set_successors([_|[]]) ->ok;

% setting the successor of each worker process,
% so the worker is able to send the token to its successor
% and the successor can consume data.
set_successors([H1|[H2|WorkersPidList]])->
	H1 ! {successorPid, H2},
	set_successors([H2|WorkersPidList]).

% creating the ring of workers and returning in the create_worker_ring
% method definition that we can find below, the list of pids
% of the created worker processes.
create_worker_ring(WorkersPidList,N) when N > 0 ->
	WorkerPID = create_worker(),
 		create_worker_ring((WorkersPidList++[WorkerPID]),N-1);

% stopping the recursion and returning the list 
% of pids for every worker process.
create_worker_ring(WorkersPidList,N) when N == 0 ->
	WorkersPidList. 	

% creating a new worker, returning its Pid generated
% when calling spawn to create this worker in a new process.
create_worker() ->
 	spawn(control, worker_loop, [self()]).

% sending a token to the desired worker (sending the token message with
% the token value to the worker, using the worker pid)	.
% Token value, here is always the same, but we add this param
% so the problem can easily integrate sending different tokens
% if desired.
send_token(WorkerPid, Token) ->
	WorkerPid ! {token, Token}.
 

% The loop of control, that adds the data consumed by each worker
% and the item that has consumed this user. This loop also stops
% all the workers once the list to consume is empty.
control_loop(WorkersPidList, [H|ListToConsume], ResultList) ->
	receive
		% a worker has eaten, we remove an item from the data consumption list
		% and add the item consumed and the pid of the worker process to a list of results.
		{Pid, eat} when ListToConsume /= [] ->
			io:format("~p eats~n",[Pid]),
			control_loop(WorkersPidList, ListToConsume, add_consumed_elem_to_res(Pid,H, ResultList));
		% adding last element of the list to consume and 
		% stopping all the workers because there is nothing
		% to consume now.
		{Pid, eat} when ListToConsume == [] ->
			io:format("~p eats~n",[Pid]),
			Result = add_consumed_elem_to_res(Pid,H, ResultList),	
				stop_workers(WorkersPidList),
				print_result(Result),
				io:format("stop~n")
	end.

% when stop_workers has notified all the workers,
% so they are able to stop (exit their worker_loop)
% we end the recursion of stopping the workers.
stop_workers([]) ->end_stop_workers;

% stops all the workers sending them a stop message
% so they exit the worker_loop
stop_workers([H|WorkersPidList])->
	H ! stop,
	stop_workers(WorkersPidList).

% adding which worker has consumed an item of the list (we add 
% its Pid), and the item that has consumed, to the result list.
add_consumed_elem_to_res(Pid, H, ResultList) ->
	ResultList ++ [{Pid,H}].

% prints the result list (the list of data to consume is empty)
% in this list we will see which workers have consumed each 
% item of the list to consume
print_result(ResultList) ->
	io:format("~p~n",[ResultList]).

% loop for every worker process. Needs SucessorWorkerPid
% in order to know who to send the token after consuming (eating)
% initially this pid is empty, because the first worker
% does not know its successor. Then we change to each worker
% the successor_id, sending a message to it with {successorPid, SuccessorPid}.
worker_loop(SucessorWorkerPid) ->
 	receive
 		% changing successorPid of this worker
 		% in order to know who to send the next token
 		% after consuming (eating). Remember that this is done
 		% because the first worker does not know his successorPid
 		% till the second worker has been created.
 		{successorPid, SuccessorPid} ->
 			worker_loop(SuccessorPid);
 		% if the worker receives the token, then
 		% he consumes (eats), send a message to the control
 		% and sends the token to his successor.	
 		% Token value, here is always the same, but we add this param
 		% so the problem can easily integrate sending different tokens
 		% if desired.
 		{token, Token} ->
 			control_process ! {self(), eat},
 			send_token(SucessorWorkerPid, Token),
 			worker_loop(SucessorWorkerPid);
 		% stopping the worker (once the control sees that the list
 		% of data to consume is empty and send this message to the worker)
 		stop ->	
 			ok
 	end.		
 	