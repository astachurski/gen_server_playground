%%%-------------------------------------------------------------------
%%% @author adrian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2015 7:50 PM
%%%-------------------------------------------------------------------
-module(fileop_system).
-author("adrian").

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).


start(_StartType, _StartArgs) ->
  case fileop_server:start() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.


stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
