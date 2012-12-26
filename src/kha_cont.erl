-module(kha_cont).

-export([start/3, start_link/3,
         attach/1, wait/1,
         exec_stream/5, exec/3,
         get_name/1,
         stop/1]).

start(Type, Name, Opts) ->
    (mod(Type)):start(Name, Opts).

start_link(Type, Name, Opts) ->
    (mod(Type)):start_link(Name, Opts).

attach(Server) ->
    gen_server:call(Server, {attach, self()}).

wait(Server) ->
    gen_server:call(Server, wait, 60000).

exec_stream(Server, Command, Ref, Parent, Opts) ->
    gen_server:call(Server, {exec_stream, Command, Ref, Parent, Opts}, infinity).

exec(Server, Command, Opts) ->
    gen_server:call(Server, {exec, Command, Opts}, infinity).

get_name(Server) ->
    gen_server:call(Server, get_name).

stop(Server) ->
    gen_server:call(Server, stop).

mod(lxc) -> kha_cont_lxc;
mod(dummy) -> kha_cont_dummy.
    
