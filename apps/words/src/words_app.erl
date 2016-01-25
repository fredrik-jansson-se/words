%%%-------------------------------------------------------------------
%% @doc words public API
%% @end
%%%-------------------------------------------------------------------

-module('words_app').

-behaviour(application).

%% Application callbacks
-export([start/2
         ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  io:format("StartType: ~p~nStartArgs: ~p~n", [_StartType, _StartArgs]),
  io:format("Priv: ~p~n", [code:lib_dir(words, priv)]),
  io:format("env: ~p~n", [application:get_env(port)]),
  'words_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
