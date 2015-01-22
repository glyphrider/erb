-module(app2_resource).
-export([
	 init/1,
	 content_types_provided/2,
	 allowed_methods/2,
	 process_post/2,
	 to_json/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

-spec allowed_methods(wrq:reqdata(), term()) -> {list(atom()), wrq:reqdata(), term()}.
allowed_methods(ReqData, State) ->
    {['GET','HEAD','POST'],ReqData,State}.

-spec content_types_provided(wrq:reqdata(), term()) -> {list(tuple(string(),atom())), wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    {[{"application/json",to_json}],ReqData,State}.

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    {"{\"sum\":5}",ReqData,State}.

process_post(ReqData,State) ->
    Body = wrq:req_body(ReqData),
    {struct,[{<<"addends">>,Addends}]} = mochijson2:decode(Body),
    Sum = app1_gen_server:add(Addends),
    {true,wrq:set_resp_body(erlang:list_to_binary(mochijson2:encode({struct,[{sum,Sum}]})),ReqData),State}.

-ifdef(EUNIT).

immutable_state_in_content_types_provided_test_() ->
    ?_test({_ContentTypeMap,_ReqData,state} = content_types_provided(rd,state)).

immutable_reqdata_in_content_types_provided_test_() ->
    ?_test({_ContentTypeMap,rd,_State} = content_types_provided(rd,state)).
    
json_supported_test_() ->
    {ContentTypeMap,rd,state} = content_types_provided(rd,state),
    ?_assert(lists:any(fun({ContentType,_ResourceFunction}) ->
			      ContentType == "application/json"
		      end,ContentTypeMap)).

allowed_methods_includes_post_test_() ->
    {AllowedMethodList,rd,state} = allowed_methods(rd,state),
    ?_assert(lists:any(fun(Method) ->
			       'POST' == Method
		       end,AllowedMethodList)).

mock_test_() ->
    {foreach,
     fun() -> Modules = [wrq,app1_gen_server],
	      meck:new(Modules),
	      Modules
     end,
     fun(Modules) ->
	     meck:unload(Modules)
     end,
     [
      {"get body",fun() ->
			  meck:expect(wrq,req_body,['_'],"{\"addends\":[1,2,3]}"),
			  meck:expect(app1_gen_server,add,[[1,2,3]],6),
			  meck:expect(wrq,set_resp_body,[<<"{\"sum\":6}">>,'_'],rd_with_body),
			  {true,rd_with_body,state} = process_post(rd,state)
		  end}
     ]}.
-endif.
