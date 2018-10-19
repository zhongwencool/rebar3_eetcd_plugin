rebar3 etcd v3 proto service generate plugin
=====

A rebar3 plugin for generating a behaviour etcd v3 service, for use with [eetcd](https://github.com/zhongwencool/eetcd).

Build
-----

```
$ rebar3 protobuf compile
$ rebar3 etcd gen
```

Use
---

Add the plugin to your rebar config:

```
{deps, [eetcd]}.

{plugins, [rebar3_gbp_plugin, rebar3_eetcd_plugin]}.
```

