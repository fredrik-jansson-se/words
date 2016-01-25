%%%-------------------------------------------------------------------
%% @doc words top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('words_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Port = case application:get_env(port) of
           {ok, P} -> P;
           _ -> 8080
         end,
  WordsFile = string:concat(code:priv_dir(words), "/words.txt"),
  Children = [
              #{ id => cowboy_words,
                 start => {cowboy_words, start_link, [{port, Port}]}
               },
              #{ id => words,
                 start => {words, start_link, [{words, WordsFile}]}
               }

             ],
  {ok, { {one_for_all, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
