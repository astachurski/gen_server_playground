%%%-------------------------------------------------------------------
%%% @author adrian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2015 8:08 PM
%%%-------------------------------------------------------------------
-module(fileop_supervisor).
-author("adrian").

-behaviour(supervisor).

%% API
-export([start_link/0, start_link_shell/0]).

%% Supervisor callbacks
-export([init/1 ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================


start_link() ->
  io:format("/n starting server\n", []),
  supervisor:start_link({global, ?SERVER}, ?MODULE, []).

start_link_shell() ->
  {ok, Pid} = supervisor:start_link({global, ?SERVER}, ?MODULE, []),
  unlink(Pid).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 3,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {fileop_server_id, {fileop_server, start_link, []},
    Restart, Shutdown, Type, [fileop_server]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
