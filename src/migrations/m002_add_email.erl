%%% File: src/migrations/m002_add_email.erl
-module(m002_add_email).
-export([id/0, up/1, down/1]).

id() -> "002".

up(_Conn) ->
    "ALTER TABLE users ADD COLUMN email TEXT;".

down(_Conn) ->
    "-- no-op for demo purposes".
