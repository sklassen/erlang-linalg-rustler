# erlang-linalg-rustler
An Erlang NIF using Rust's Rustler and nalgebra

Uses rust nalgebra and rustlers to provide linalg functions.

Assuming you have rustup, erlang and rebar3. 

```
rebar3 shell
```

Should build and launch 

```
Erlang/OTP 23 [erts-11.1.8] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V11.1.8  (abort with ^G)
1> {U,S,Vt}=linalg_ruslin:svd([[3.0,2.0],[2.0,3.0]]).
{[[0.7071067811865474,0.7071067811865475],
  [0.7071067811865475,-0.7071067811865476]],
 [5.0,1.0000000000000002],
 [[0.7071067811865477,0.7071067811865475],
  [0.7071067811865475,-0.7071067811865477]]}
2> 
```
