-module(helloworld).
-export([start/0]).

start() ->
   io:fwrite("~w",[1+1]).