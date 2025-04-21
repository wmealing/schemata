
%%% File: src/my_migrator_repo.erl
-module(my_migrator_repo).
-export([connect/2, close/2, ensure_tracking_table/2, applied_migrations/2,
         record_migration/3, remove_migration/3]).

connect(Path, Adapter) ->
    Adapter:connect(Path).

close(Conn, Adapter) ->
    Adapter:close(Conn).

ensure_tracking_table(Conn, Adapter) ->
    Adapter:ensure_tracking_table(Conn).

applied_migrations(Conn, Adapter) ->
    Adapter:applied_migrations(Conn).

record_migration(Conn, Id, Adapter) ->
    Adapter:record_migration(Conn, Id).

remove_migration(Conn, Id, Adapter) ->
    Adapter:remove_migration(Conn, Id).
