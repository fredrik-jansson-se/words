%%%-------------------------------------------------------------------
%%% @author frjansso
%%% @copyright (C) 2016, frjansso
%%% @doc
%%%
%%% @end
%%% Created : 2016-01-08 14:44:51.219372
%%%-------------------------------------------------------------------
-module(words).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1,
        match/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          words
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

match(Word) ->
  gen_server:call(?SERVER, {match, Word}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{words, WordsFile}]) ->
  io:format("Reading ~p~n", [WordsFile]),
  case file:open(WordsFile, [read]) of 
    {ok, IoDevice} ->
      Words = get_all_lines(IoDevice, []),
      io:format("Read ~p words~n", [length(Words)]),
      {ok, #state { words = Words }};
    {error, Reason} ->
      {stop, Reason}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({match, Word}, _From, State) ->
  {reply, {ok, match(Word, State#state.words)}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec word_to_regexp(binary()) -> string().
word_to_regexp(Word) ->
  R = word_to_regexp(binary_to_list(Word), ""),
  list_to_binary(R).

-spec word_to_regexp(string(), string()) -> string().
word_to_regexp("", Acc) ->
  lists:reverse(Acc);
word_to_regexp([H|T], Acc) when H >= $a andalso H =< $z ->
  word_to_regexp(T, [$.|Acc]);
word_to_regexp([H|T], Acc) ->
  LC = H - $A + $a,
  word_to_regexp(T, [LC|Acc]).


word_to_regexp_test() ->
  <<"f..">> = word_to_regexp(<<"Foo">>),
  <<"f..b">> = word_to_regexp(<<"FooB">>),
  <<"foo">> = word_to_regexp(<<"FOO">>),
  ok.

-spec match(string(), [string()]) -> [string()].
match(Word, Words) ->
  {ok, RegEx} = re:compile(word_to_regexp(Word)),
  match(RegEx, Words, []).

%-spec match(re:mp(), [string()], [string()]) -> [string()].
match(_RegEx, [], Acc) -> 
  Acc;
match(RegEx, [W|Words], Acc) ->
  case re:run(W, RegEx) of
    {match, _} -> match(RegEx, Words, [W|Acc]);
    _ -> 
      match(RegEx, Words, Acc)
  end.


match_test() ->
  Words = [<<"foo">>, <<"fee">>, <<"foo">>, <<"woo">>, <<"wow">>],
  [<<"wow">>, <<"woo">>] = match(<<"Woo">>, Words),
  [<<"woo">>] = match(<<"WoO">>, Words),
  ok.

get_all_lines(IoDevice, Acc) ->
  case io:get_line(IoDevice, "") of
    eof -> file:close(IoDevice), lists:reverse(Acc);
    Line -> get_all_lines(IoDevice, [list_to_binary(string:strip(Line, both, $\n)) | Acc])
  end.

main_test() ->
  start_link({words, "apps/words/priv/words.txt"}),
  W1 = <<"teCciRkoeeF">>,
  R1 = match(W1),
  ?debugFmt("~p ~p~n", [W1, R1]),
  ?debugFmt("~p~n", [match(<<"HOtosdontuH">>)]),
  ok.


