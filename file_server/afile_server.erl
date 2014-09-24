% module name
-module(afile_server).
% author
-author("Jonathan Fragoso").
% version
-vsn(1).
% list of the methods (functions) that can be called from other modules (public functions, all the functions
% that are not here in the export, cannot be called outside this module; are private)
% NOTE --> the number on the right of the method name, is the number of the arguments that has this function
-export([start/1]).

% start function
start(Dir) ->
	% calling the primitive spawn in order to create a new parallel 
	% process which will run the loop method with Dir as a parameter
	spawn(afile_server, loop, [Dir]).

% loop
loop(Dir) ->
	% waits for a new client request
	receive
		% if we receive the message {Client, list_dir} we will reply with a list of files
		% else, if we receive the message {Client, {get_file, File}} we reply with that file
		% Those two cases are called patterns and are separated with ';'. No need to if-then-else or switch
		% because Erlang works on pattern matching.	
		% Client --> Variable, process identifier of the process that sent the request and to whom the
		% reply should be sent
		{Client, list_dir} ->
			Client ! {self(), file:list_dir(Dir)};   % self() means the process identifier of the server
								 % (client can check that the message the client
								 % received comes from the server and not some 
								 % other process.
		{Client, {get_file, File}} ->
			Full = filename:join(Dir, File),
			Client ! {self(), file:read_file(Full)}	
	end,

	% way to write an infinite loop in Erlang, here we're calling again same function in order 
	% to listen to new client requests
	loop(Dir).	


