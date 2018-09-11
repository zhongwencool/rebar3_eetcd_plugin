%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd {{unmodified_service_name}}.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on {{datetime}} and should not be modified manually

-module(eetcd_{{module_name}}).

{{#methods}}
-export([{{method}}/1]).
{{/methods}}

{{#methods}}
%% @doc {{^output_stream}}{{^input_stream}}Unary RPC {{/input_stream}}{{#input_stream}}Stream RPC {{/input_stream}}{{/output_stream}}
-spec {{method}}({{pb_module}}:'{{input}}'()) ->
    {{^output_stream}}{{^input_stream}}{ok, {{pb_module}}:'{{output}}'()}{{/input_stream}}{{#input_stream}}reference(){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}reference(){{/input_stream}}{{#input_stream}}reference(){{/input_stream}}{{/output_stream}} | {error, term()}.
{{method}}(Request) ->
    {{^output_stream}}{{^input_stream}}eetcd_stream:unary(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>, '{{output}}'){{/input_stream}}{{#input_stream}}eetcd_stream:data(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>){{/input_stream}}{{/output_stream}}{{#output_stream}}{{^input_stream}}eetcd_stream:data(Request, <<"/etcdserverpb.{{unmodified_service_name}}/{{unmodified_method}}">>){{/input_stream}}{{/output_stream}}.

{{/methods}}