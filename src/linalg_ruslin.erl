-module(linalg_ruslin).
-export([add/2]).

-on_load(init/0).


init() ->
  Directory=filename:dirname(code:which(linalg_ruslin)),
	erlang:load_nif(Directory++"/../priv/libruslin", 0).

add(_,_) -> 
        exit(nif_library_not_loaded).


