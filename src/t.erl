-module(t).

-compile([export_all]).

-record(handler_state, {out = user,
                        max_events = inf}).

-include_lib("stdlib/include/ms_transform.hrl").

ut() ->
    dbg:ctp().

t(Mod)
  when is_atom(Mod) ->
    t([Mod]);

t({Mod, Fun})
  when is_atom(Mod),
       is_atom(Fun) ->
    t([{Mod, Fun}]);

t(What) ->
    t0(What, []).

t(What, N) when is_integer(N) ->
    t0(What, [process, {fun dhandler/2, #handler_state{max_events = N}}]).

t0(What, Args) ->
    apply(dbg, tracer, Args),
    dbg:p(all, [call]),
    t1(What, dbg:fun2ms(fun(_) -> return_trace() end)).

ts(What) ->
    dbg:tracer(),
    dbg:p(all, [call]),
    t1(What, []).

t1([], _Flags) ->
    ok;

t1([{M, F}|T], Flags) ->
    dbg:tpl(M, F, Flags),
    t1(T, Flags);

t1([H|T], Flags) ->
    dbg:tpl(H, Flags),
    t1(T, Flags).

apps2procs(Apps) ->
    Apps2 = [{ok,A} || A <- Apps],
    [P || P <- processes(), lists:member(application:get_application(P), Apps2) ].

gc() ->
    [ erlang:garbage_collect(P) || P <- processes() ].
    
topcpu() ->
    topcpu(processes()).
topcpu(Procs) ->
    topcpu(Procs, 10).
topcpu(Procs, Limit) ->
    toppi(Procs, Limit, reductions).

topmsgq() ->
    topmsgq(processes()).
topmsgq(Procs) ->
    topmsgq(Procs, 10).
topmsgq(Procs, Limit) ->
    toppi(Procs, Limit, message_queue_len).

topmem() ->
    topmem(processes()).
topmem(Procs) ->
    topmem(Procs, 10).
topmem(Procs, Limit) ->
    toppi(Procs, Limit, total_heap_size).

toppi(Procs, Limit, Info) ->
    L = lists:flatmap(fun(P) ->
                              try
                                  [{P, element(2, erlang:process_info(P, Info))}]
                              catch
                                  _:_ ->
                                      []
                              end
                      end, Procs),
    lists:sublist(lists:reverse(lists:keysort(2, L)), Limit).

mods2procs(Mods) ->
    [P || M <- Mods, P <- processes(), erlang:check_process_code(P, M)].

p(Mod) ->
    p(Mod, processes(), []).

p(_, [], Acc) ->
    Acc;

p(Mod, [H|T], Acc) ->
    {_, L} = process_info(H, dictionary),
    case lists:keyfind('$initial_call', 1, L) of
        {'$initial_call', {Mod, init, 1}} ->
            p(Mod, T, [H|Acc]);
        _ ->
            p(Mod, T, Acc)
    end.

ns(State) ->
    L = [P || {_, P, _, _} <- supervisor:which_children(oortle_flash_sup) ],
    n(State, L).

np(State) ->
    L = p(flashbot),
    n(State, L).

n(State, L) ->
    [ oortle_info(P) || P <- L, state_is_not(State, P) ].

state_is_not(State, P) ->
    {_, L} = process_info(P, dictionary),
    case lists:keyfind(oortle_state, 1, L) of
        {oortle_state, State} ->
            false;
        _ ->
            true
    end.

oortle_info(P) ->
    {_, L} = process_info(P, dictionary),
    In = proplists:get_value(oortle_in, L, []),
    Out = proplists:get_value(oortle_out, L, []),
    State = proplists:get_value(oortle_state, L),
    [{pid, P},
     {state, State},
     {in, lists:reverse(In)},
     {out, lists:reverse(Out)}
    ].

state(State) ->
    put(oortle_state, State).

save_packet(Key, Packet)
  when is_list(Packet) ->
    save_packet(Key, iolist_to_binary(Packet));

save_packet(Key, Packet) ->
    case get(Key) of
        undefined ->
            put(Key, [Packet]);
        [A, B, _] ->
            put(Key, [Packet, A, B]);
        L ->
            put(Key, [Packet|L])
    end.

last_in(Packet) ->
    save_packet(oortle_in, Packet).

last_out(Packet) ->
    save_packet(oortle_out, Packet).

get(P, K) ->
    {_, L} = process_info(P, dictionary),
    proplists:get_value(K, L).

pi(P)
  when is_pid(P) ->
    rpc:call(node(P), erlang, process_info, [P]).

msg_watch(P) ->
    spawn(fun() ->
                  msg_watch1(P)
          end).

msg_watch1(P) ->
    I = pi(P),
    timer:sleep(300),
    io:format("~p~n", [lists:keyfind(message_queue_len, 1, I)]),
    io:format("~p~n", [lists:keyfind(messages, 1, I)]),
    msg_watch1(P).







dec(#handler_state{max_events = 1} = _State) ->
    dbg:stop();
dec(#handler_state{max_events = N} = State) ->
    State#handler_state{max_events = N-1}.


dhandler(end_of_trace, #handler_state{} = State) ->
    State;
dhandler(Trace, #handler_state{} = State) when element(1, Trace) == trace, tuple_size(Trace) >= 3 ->
    dhandler1(Trace, tuple_size(Trace), State);
dhandler(Trace, #handler_state{} = State) when element(1, Trace) == trace_ts, tuple_size(Trace) >= 4 ->
    dhandler1(Trace, tuple_size(Trace)-1, element(tuple_size(Trace),Trace), State);
dhandler(Trace, #handler_state{out = Out} = State) when element(1, Trace) == drop, tuple_size(Trace) =:= 2 ->
    io:format(Out, "*** Dropped ~p messages.~n", [element(2,Trace)]),
    State;
dhandler(Trace, #handler_state{out = Out} = State) when element(1, Trace) == seq_trace, tuple_size(Trace) >= 3 ->
    SeqTraceInfo = case Trace of
		       {seq_trace, Lbl, STI, TS} ->
			   io:format(Out, "SeqTrace ~p [~p]: ",
				     [TS, Lbl]),
			   STI;
		       {seq_trace, Lbl, STI} ->
			  io:format(Out, "SeqTrace [~p]: ",
				     [Lbl]),
			   STI
		   end,
    case SeqTraceInfo of
	{send, Ser, Fr, To, Mes} ->
	    io:format(Out, "(~p) ~p ! ~p [Serial: ~p]~n",
		      [Fr, To, Mes, Ser]);
	{'receive', Ser, Fr, To, Mes} ->
	    io:format(Out, "(~p) << ~p [Serial: ~p, From: ~p]~n",
		      [To, Mes, Ser, Fr]);
	{print, Ser, Fr, _, Info} ->
	    io:format(Out, "-> ~p [Serial: ~p, From: ~p]~n",
		      [Info, Ser, Fr]);
	Else ->
	    io:format(Out, "~p~n", [Else])
    end,
    dec(State);
dhandler(_Trace, #handler_state{} = State) ->
    State.

dhandler1(Trace, Size, #handler_state{out = Out} = State) ->
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message ->
		    io:format(Out, "(~p) << ~p~n", [From,Message])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    To = element(5, Trace),
	    io:format(Out, "(~p) ~p ! ~p~n", [From,To,Message]);
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(Out, "(~p) call ~s (~p)~n", [From,ffunc(MFA),Message]);
		MFA ->
		    io:format(Out, "(~p) call ~s~n", [From,ffunc(MFA)])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(Out, "(~p) old_ret ~s -> ~p~n", [From,ffunc(MFA),Ret]);
		MFA ->
		    io:format(Out, "(~p) old_ret ~s~n", [From,ffunc(MFA)])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(Out, "(~p) returned from ~s -> ~p~n", [From,ffunc(MFA),Ret]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(Out, "(~p) returning to ~s~n", [From,ffunc(MFA)]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Out, "(~p) spawn ~p as ~s~n", [From,Pid,ffunc(MFA)]);
	Op ->
	    io:format(Out, "(~p) ~p ~s~n", [From,Op,ftup(Trace,4,Size)])
    end,
    dec(State).

dhandler1(Trace, Size, TS, #handler_state{out = Out} = State) ->
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message ->
		    io:format(Out, "(~p) << ~p (Timestamp: ~p)~n", [From,Message,TS])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    To = element(5, Trace),
	    io:format(Out, "(~p) ~p ! ~p (Timestamp: ~p)~n", [From,To,Message,TS]);
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(Out, "(~p) call ~s (~p) (Timestamp: ~p)~n", [From,ffunc(MFA),Message,TS]);
		MFA ->
		    io:format(Out, "(~p) call ~s (Timestamp: ~p)~n", [From,ffunc(MFA),TS])
	    end;
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(Out, "(~p) old_ret ~s -> ~p (Timestamp: ~p)~n", [From,ffunc(MFA),Ret,TS]);
		MFA ->
		    io:format(Out, "(~p) old_ret ~s (Timestamp: ~p)~n", [From,ffunc(MFA),TS])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(Out, "(~p) returned from ~s -> ~p (Timestamp: ~p)~n", [From,ffunc(MFA),Ret,TS]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(Out, "(~p) returning to ~s (Timestamp: ~p)~n", [From,ffunc(MFA),TS]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Out, "(~p) spawn ~p as ~s (Timestamp: ~p)~n", [From,Pid,ffunc(MFA),TS]);
	Op ->
	    io:format(Out, "(~p) ~p ~s (Timestamp: ~p)~n", [From,Op,ftup(Trace,4,Size),TS])
    end,
    dec(State).



%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl}) when is_list(Argl) ->
    io_lib:format("~p:~p(~s)", [M, F, fargs(Argl)]);
ffunc({M,F,Arity}) ->
    io_lib:format("~p:~p/~p", [M,F,Arity]);
ffunc(X) -> io_lib:format("~p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity) when is_integer(Arity) -> integer_to_list(Arity);
fargs([]) -> [];
fargs([A]) -> io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~p,", [A]) | fargs(Args)];
fargs(A) -> io_lib:format("~p", [A]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index) ->
    io_lib:format("~p", [element(Index, Trace)]);
ftup(Trace, Index, Size) ->
    [io_lib:format("~p ", [element(Index, Trace)])
     | ftup(Trace, Index+1, Size)].
