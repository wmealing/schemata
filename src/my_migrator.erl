%%% File: src/my_migrator.erl
-module(my_migrator).
-export([migrate/2, rollback/2]).

migrate(DbPath, Adapter) ->
    {ok, Conn} = my_migrator_repo:connect(DbPath, Adapter),
    ok = my_migrator_runner:apply_pending(Conn, Adapter),
    ok = my_migrator_repo:close(Conn, Adapter).

rollback(DbPath, Adapter) ->
    {ok, Conn} = my_migrator_repo:connect(DbPath, Adapter),
    ok = my_migrator_runner:rollback_last(Conn, Adapter),
    ok = my_migrator_repo:close(Conn, Adapter).
