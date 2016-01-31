-module(comments_handler).

-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         hello_to_json/2,
         post_comment/2
        ]).
-record(state, {
          json
         }).


init(Req, _Opts) ->
  Comments = comments_file(),
  {ok, JsonData} = file:read_file(Comments),
  Json = jsx:decode(JsonData),
  {cowboy_rest, Req, #state{json=Json}}.


allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, '*'}, hello_to_json}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, post_comment}
   ], Req, State}.

hello_to_json(Req, State) ->
  Json = State#state.json,
  Body = jsx:encode(Json),
  {Body, Req, State}.

post_comment(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body_qs(Req),
  Json = State#state.json,
  NewJson = [Body | Json],
  ok = file:write_file(comments_file(), jsx:encode(NewJson)),
  NewState = State#state{json = NewJson},
  {true, Req2, NewState}.


comments_file() ->
  string:concat(code:priv_dir(words),"/comments.json").
