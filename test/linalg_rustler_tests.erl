-module(linalg_rustler_tests). 
-import(linalg_ruslin,[transpose/1,inv/1,matmul/2]).
-include_lib("eunit/include/eunit.hrl").

transpose_1_test() ->
	?assertEqual([[8.0]],transpose([[8.0]])).

transpose_2_test() ->
	?assertEqual([[1.0,3.0],[2.0,4.0]],transpose([[1.0,2.0],[3.0,4.0]])).

transpose_3_test() ->
	?assertEqual([[1.0,4.0],[2.0,5.0],[3.0,6.0]],transpose([[1.0,2.0,3.0],[4.0,5.0,6.0]])).

inv_1_test()->
	?assertEqual([[0.125]],inv([[8.0]])).

inv_2_test()->
	?assertEqual([[1.0,0.0],[0.0,0.5]],inv([[1.0,0.0],[0.0,2.0]])).

inv_3_test()->
	?assertEqual([[-1.0,-1.0,2.0],[-1.0,0.0,1.0],[2.0,1.0,-2.0]],inv([[1.0,0.0,1.0],[0.0,2.0,1.0],[1.0,1.0,1.0]])).

matmul_test()->
    [
    ?assertEqual([[6.0]], matmul([[2.0]], [[3.0]])),
    ?assertEqual([[5.0,11.0],[11.0,25.0]], matmul([[1.0,2.0],[3.0,4.0]], [[1.0,3.0],[2.0,4.0]]))
    ].

