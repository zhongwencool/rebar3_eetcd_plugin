%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd {{unmodified_service_name}}.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on {{datetime}} and should not be modified manually

-module(eetcd_{{module_name}}).

-include("router_pb.hrl").
-include("eetcd.hrl").
{{#methods}}
-export([{{method}}/1, {{method}}/2]).
{{/methods}}

{{#methods}}
%% @doc {{^output_stream}}{{^input_stream}}Unary RPC {{/input_stream}}{{#input_stream}}Stream RPC {{/input_stream}}{{/output_stream}}
-spec {{method}}(#'{{input}}'{}) ->
    {{^output_stream}}{{^input_stream}}{ok, #'{{output}}'{}}{{/input_stream}}{{#input_stream}}reference(){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}reference(){{/input_stream}}{{#input_stream}}reference(){{/input_stream}}{{/output_stream}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
{{method}}(Request) when is_record(Request, '{{input}}') ->
    {{^output_stream}}{{^input_stream}}eetcd_stream:unary(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, '{{output}}', []){{/input_stream}}{{#input_stream}}eetcd_stream:new(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, []){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}eetcd_stream:new(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, []){{/input_stream}}{{/output_stream}}.
-spec {{method}}(#'{{input}}'{}, Http2Headers) ->
    {{^output_stream}}{{^input_stream}}{ok, #'{{output}}'{}}{{/input_stream}}{{#input_stream}}reference(){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}reference(){{/input_stream}}{{#input_stream}}reference(){{/input_stream}}{{/output_stream}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
{{method}}(Request, Token) when is_record(Request, '{{input}}') andalso is_binary(Token) ->
    {{^output_stream}}{{^input_stream}}eetcd_stream:unary(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, '{{output}}', [{<<"authorization">>, Token}]){{/input_stream}}{{#input_stream}}eetcd_stream:new(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, [{<<"authorization">>, Token}]){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}eetcd_stream:new(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, [{<<"authorization">>, Token}]){{/input_stream}}{{/output_stream}};
{{method}}(Request, Http2Headers) when is_record(Request, '{{input}}') andalso is_list(Http2Headers) ->
   {{^output_stream}}{{^input_stream}}eetcd_stream:unary(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, '{{output}}', Http2Headers){{/input_stream}}{{#input_stream}}eetcd_stream:new(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, Http2Headers){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}eetcd_stream:new(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, Http2Headers){{/input_stream}}{{/output_stream}}.

{{/methods}}
