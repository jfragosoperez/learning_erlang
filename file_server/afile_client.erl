-module(afile_client).
-author("Jonathan Fragoso").
-vsn(1).
-export([ls/1, get_file/2, put_file/1]).

ls(Server) ->
	Server ! {self(), list_dir},
	receive
		{Server, FileList} ->
			FileList
	end.

get_file(Server, File) ->
	Server ! {self(), {get_file, File}},
	receive
		{Server, Content} ->
			Content
	end.

put_file(Source) ->
	io:format("Putting file into server~n").



