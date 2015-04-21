%%%-------------------------------------------------------------------
%%% @author adrian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2015 7:53 PM
%%%-------------------------------------------------------------------


{application, fileop_system, [
  {description, "remote file manipulation"},
  {vsn, "1"},
  {module, [fileop_logic, fileop_server, fileop_system]},
  {registered, []},

  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {fileop_system, []}}, %entry point
  {env, []}
]}.