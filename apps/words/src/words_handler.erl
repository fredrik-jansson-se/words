-module(words_handler).

-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         get_word_results/2
        ]).

init(Req, _Opts) ->
  {cowboy_rest, Req, {}}.


allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, '*'}, get_word_results}
  ], Req, State}.


resource_exists(Req, State) ->
  % Check that a word is provided
  Res = case cowboy_req:binding(word, Req) of
          undefined -> false;
          _ -> true
        end,
  {Res, Req, State}.

qs_to_opts([{<<"match_start">>, <<"true">>}|T], Opts) ->
  qs_to_opts(T, [match_start|Opts]);
qs_to_opts([{<<"match_end">>, <<"true">>}|T], Opts) ->
  qs_to_opts(T, [match_end|Opts]);
qs_to_opts([H|T], Opts) ->
  io:format("Skipping unknown opt ~p~n", [H]),
  qs_to_opts(T, Opts);
qs_to_opts([], Opts) ->
  Opts.

get_word_results(Req, State) ->
  QS = cowboy_req:parse_qs(Req),
  io:format("QS: ~p~n", [QS]),
  Word = cowboy_req:binding(word, Req),
  {ok, Words} = words:match(Word, qs_to_opts(QS, [])),
  Res = jsx:encode(Words),
  {Res, Req, State}.
