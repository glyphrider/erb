-module(app2_resource).
-export([
	 init/1,
	 content_types_provided/2,
	 to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[{"application/json",to_json}],ReqData,State}.

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    {"{\"sum\":5}",ReqData,State}.
