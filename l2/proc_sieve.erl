-module(proc_sieve).
-author("longlom").

%% API
-export([result/0, generate/1, sieve/1]).


sieve(State) ->
  {N, NextPid} = State,
  receive
    Value when is_integer(Value) ->
      if
        N == undefined ->
          io:format("Value ~w set for  ~w~n",[Value, self()]),
          NewN = Value,
          NewState = {NewN, NextPid},
          sieve(NewState);
        true ->
          Remainder = Value rem N,
          if
            Remainder == 0 ->
              io:format("~w passed to ~n",[Value]),
              sieve(State);
            true ->
              if
                NextPid == undefined ->
                  InitialState = {undefined, undefined},
                  NewNextPid = spawn(proc_sieve, sieve, [InitialState]),
                  NewNextPid ! Value,
                  io:format("~w new thread ~w~n",[Value, NewNextPid]),
                  NewState = {N, NewNextPid},
                  sieve(NewState);
                true ->
                  NextPid ! Value,
                  io:format("~w sent to next thread ~w~n",[Value, NextPid]),
                  sieve(State)
              end
          end
      end;
    {done, RepId} ->
      if
        NextPid == undefined ->
          RepId ! [N],
          io:format("~w was killed~n",[self()]),
          exit(normal);
        true -> NextPid ! {done, self()},
          receive
            Value when is_list(Value) ->
              RepId ! [N|Value],
              io:format("Process ~w was killed for medium process~n",[self()]),
              exit(normal)
          end
      end
  end.

generate(MaxN) ->
  List = lists:seq(2, MaxN),
  InitialState = {undefined, undefined},
  BasePid = spawn(proc_sieve, sieve, [InitialState]),
  lists:foreach(fun(X) -> BasePid ! X end, List),
  BasePid ! {done, self()},
  receive
    Value -> Value
  end.


result() -> io:format("~w~n", [generate(1000)]).
