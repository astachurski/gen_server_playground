-module(testserv).
-author("adrian").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  sssd/0]).

%-define(SERVER, ?MODULE).

-record(state, {}).

%% =========== initialization api =================

sssd() ->
  start_link(),
  suckdick().


start_link() ->
  gen_server:start_link({global,?MODULE}, ?MODULE, [], []).


%% ==== external api ===================================

suckdick() -> gen_server:call({global,?MODULE}, suckdick).


%% ========== rpc =============================


init([]) ->
  {ok, #state{}}.


handle_call(suckdick, _From, State) ->
  {reply, sucked, State};


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

