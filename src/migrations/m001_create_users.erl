
%%% File: src/migrations/m001_create_users.erl
-module(m001_create_users).
-export([id/0, up/1, down/1]).

id() -> "001".

%% maybe i dont need the conn here.
up(_Conn) ->
    "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);".

down(_Conn) ->
    "DROP TABLE users;".

