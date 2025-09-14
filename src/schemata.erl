%%% File: src/schemata.erl
-module(schemata).
-export([migrate/1, rollback/1]).

-export([connect/2, close/1, ensure_tracking_table/2, applied_migrations/2,
         record_migration/3, remove_migration/3]).

-include_lib("eunit/include/eunit.hrl").

migrate(#{adapter := Adapter, pid := Pid} = ConnectionInfo) ->
    {ok, Conn} = schemata:connect(ConnectionInfo, Adapter),
    ok = schemata_runner:apply_pending(Conn, Adapter),
    schemata:close(Conn).

rollback(#{adapter := Adapter} = ConnectionInfo) ->
    {ok, Conn} = connect(ConnectionInfo, Adapter),
    ok = schemata_runner:rollback_last(Conn, Adapter),
    close(Conn).

connect(ConnectionInfo, Adapter) ->
    {ok, Connected} = Adapter:connect(ConnectionInfo),
    {ok, Connected#{adapter => Adapter}}.

close(#{adapter := Adapter} = ConnectionInfo) ->
%%    ?debugFmt ("CONNECTION INFO CLOSE: ~p~n", [ConnectionInfo]),
    Adapter:close(ConnectionInfo).

ensure_tracking_table(Conn, Adapter) ->
    Adapter:ensure_tracking_table(Conn).

applied_migrations(Conn, Adapter) ->
    Adapter:applied_migrations(Conn).

record_migration(Conn, Id, Adapter) ->
    Adapter:record_migration(Conn, Id).

remove_migration(Conn, Id, Adapter) ->
    Adapter:remove_migration(Conn, Id).
