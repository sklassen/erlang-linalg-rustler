-module(linalg_ruslin).
-export([version/0,add/2,sum/1,inv/1,matmul/2,transpose/1,svd/1,diag/1]).

-on_load(init/0).


init() ->
  Directory=filename:dirname(code:which(linalg_ruslin)),
	erlang:load_nif(Directory++"/../priv/libruslin", 0).

version() -> 
        exit(nif_library_not_loaded).

matmul(_,_) -> 
        exit(nif_library_not_loaded).

add(_,_) -> 
        exit(nif_library_not_loaded).

sum(_) -> 
        exit(nif_library_not_loaded).

transpose(_) -> 
        exit(nif_library_not_loaded).

inv(_) -> 
        exit(nif_library_not_loaded).

diag(_) -> 
        exit(nif_library_not_loaded).

svd(_) -> 
        exit(nif_library_not_loaded).

