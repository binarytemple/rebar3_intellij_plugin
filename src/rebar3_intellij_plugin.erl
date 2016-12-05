%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2016 Bryan Hunt (admin@binarytemple.co.uk)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------

%% The rebar_intellij_compiler module is a plugin for rebar that generates
%% intellij project configuration files. This is probably easier than trying to 
%% write a compiler/build plugin for intellij which supports rebar3.
%%

-module(rebar3_intellij_plugin).

-behaviour(provider).

-export([init/1,
  do/1,
  format_error/1]).

-define(PROVIDER, generate).
-define(DEPS, [{

%%  compile,        default,
  app_discovery

  %% , lock

}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
    {module, ?MODULE},
    {namespace, intellij},
    {bare, false},
    {deps, ?DEPS},
    {example, "rebar3 intellij compile"},
    {short_desc, "generate intellij config files."},
    {desc, "generate intellij config files."},
    {opts, [intellij]}])),


%%file:write_file("/tmp/dir.txt",  [io_lib:format("~p",[rebar_dir:recursive(rebar_state:opts(State1,"foo"))])]),
  {ok, State1}.

do(State1) ->
  rebar_api:info("Running intellij...", []),

  rebar_file_utils:write_file_if_contents_differ("/tmp/state.txt", [io_lib:format("~p", [State1])]),
  rebar_file_utils:write_file_if_contents_differ("/tmp/tag.txt", [io_lib:format("~p", [os:timestamp()])]),
  rebar_file_utils:write_file_if_contents_differ("/tmp/deps.txt", [io_lib:format("~p", [rebar_state:code_paths(State1, all_plugin_deps)])]),
  rebar_file_utils:write_file_if_contents_differ("/tmp/args.txt", [io_lib:format("~p", [rebar_state:command_parsed_args(State1)])]),
%% file:write_file("/tmp/apps.txt", [io_lib:format("~p",[rebar_state:project_apps(State1)])]),

  rebar_file_utils:write_file_if_contents_differ("/tmp/bstate.txt", [term_to_binary(State1)]),

  rebar_file_utils:write_file_if_contents_differ("/tmp/dir.txt",
    [io_lib:format("~p", [
      {
        {all_src_dirs, rebar_dir:all_src_dirs(rebar_state:opts(State1))} ,
        {checkouts_dir, rebar_dir:checkouts_dir(State1)} ,
{deps_dir, rebar_state:all_deps(State1)}
      }
    ])]),

  Apps = case rebar_state:current_app(State1) of
           undefined ->
             rebar_state:project_apps(State1);
           AppInfo ->
             [AppInfo]
         end,
  rebar_file_utils:write_file_if_contents_differ("/tmp/apps.txt", [io_lib:format("~p", [Apps])]),

  Res = [begin
           Opts = rebar_app_info:opts(AppInfo),
           OutDir = rebar_app_info:out_dir(AppInfo),
           Dir = rebar_app_info:dir(AppInfo),
           ErlFiles = rebar_utils:find_files(Dir, ".*\\.erl\$"),
           HrlFiles = rebar_utils:find_files(Dir, ".*\\.hrl\$"),
           RebarConfigFiles = rebar_utils:find_files(Dir, "rebar\\.config\$"),

           [
             {opts, Opts},
             {outdir, OutDir},
             {sourcedir, Dir},
             {erl_files, ErlFiles},
             {hrl_files, HrlFiles},
             {app_config_files, RebarConfigFiles}
           ]
         end || AppInfo <- Apps],

  rebar_file_utils:write_file_if_contents_differ("/tmp/res.txt", [io_lib:format("~p", [Res])]),
  rebar_file_utils:write_file_if_contents_differ("/tmp/bres.txt", term_to_binary(Res)),

  rebar_api:warn("root : ~p", [  rebar_dir:root_dir(State1) ]),
  rebar_api:warn("processing base dir: ~p", [maybe_make_idea_dir(   rebar_dir:root_dir(State1))]),



  HrlDirs = [ D || D <- lists:usort ( [ filename:dirname(F) || F <- proplists:get_value(hrl_files, C) ] ), lists:suffix("include",D) ].


  {ok, State1}.

maybe_make_idea_dir(RootDir) ->
 Target =  filename:join(RootDir, ".idea.tmp") ,
 case filelib:is_dir(Target) of
  true -> ok;
 false -> file:make_dir(Target)
end
.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

