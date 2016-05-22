-module(afile_client).
-export([ls/1, get_file/2, put_file/3]).

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

put_file(Server, Filename, Content) ->
  Server ! {self(), {put_file, Filename, Content}},
  receive
    {Server, ok} ->
      io:fwrite("File saved on Server~n");
    {Server, Err} ->
      io:fwrite("Error occured on the server~s~n", [Err])
  end.