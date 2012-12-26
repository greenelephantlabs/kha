-module(kha_lang).

-export([set_runtime_cmd/2]).

mod(Atom) ->
    list_to_atom("kha_lang_" ++ atom_to_list(Atom)).

set_runtime_cmd(Lang, Ver) ->
    (mod(Lang)):set_runtime_cmd(Ver).
