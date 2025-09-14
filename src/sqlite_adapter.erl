-module(sqlite_adapter).

-export([connect/1, close/1, ensure_tracking_table/1]).
-export([exec/2, applied_migrations/1, record_migration/2, remove_migration/2]).

-include_lib("eunit/include/eunit.hrl").

connect(#{file := Path, dbname := DbName} = ConnectionInfo) ->
    {ok, Pid} = sqlite3:open(DbName, [{file, Path}]),
    {ok, ConnectionInfo#{pid => Pid}}.

close(#{pid := Pid} = ConnectionInfo) ->
    sqlite3:close(Pid),
    {ok, maps:without([pid], ConnectionInfo)}.

ensure_tracking_table(Conn) ->
    Pid = maps:get(pid, Conn),
    Sql = "CREATE TABLE IF NOT EXISTS schema_migrations (id TEXT PRIMARY KEY, applied_at TEXT);",
    exec(Pid, Sql).

applied_migrations(Pid) ->
    Sql = "SELECT id FROM schema_migrations ORDER BY id;",
    [_ , {rows, Rows}] = exec (Pid, Sql),
    [ binary_to_list(element(1,Id)) || Id <- Rows].

record_migration(Conn, Id) ->
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_universal_time(erlang:timestamp()),
    Timestamp = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                               [Y, M, D, H, Min, S]),
    Sql = io_lib:format("INSERT INTO schema_migrations (id, applied_at) VALUES ('~s', '~s');",
                        [Id, lists:flatten(Timestamp)]),
    exec(Conn, lists:flatten(Sql)).

remove_migration(Conn, Id) ->
    Sql = io_lib:format("DELETE FROM schema_migrations WHERE id = '~s';", [Id]),
    exec(Conn, lists:flatten(Sql)).

exec(Conn, Sql) ->
    sqlite3:sql_exec(Conn, Sql).
