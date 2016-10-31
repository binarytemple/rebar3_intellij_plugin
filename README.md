rebar3_intellij_plugin
=====

A rebar plugin

Pre-beta... Don't bother using yet. 

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_intellij_plugin, ".*", {git, "https://github.com/binarytemple/rebar3_intellij_plugin.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 intellij generate
    ===> Fetching rebar3_erlydtl_plugin
    ===> Compiling rebar3_erlydtl_plugin

To have it invoked automatically when running `rebar3 compile` add it as a `provider_hooks`:

```
{provider_hooks, [
                 {pre, [{compile, {intellij, generate}}]}
                 ]}.
```
