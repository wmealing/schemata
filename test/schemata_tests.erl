%%% @author Wade Mealing <wmealing@Wades-MacBook-Air.local>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2025 by Wade Mealing <wmealing@Wades-MacBook-Air.local>

-module(schemata_tests).

-include_lib("eunit/include/eunit.hrl").

top_setup() ->
    file:delete("something.db", []).

top_cleanup(_) ->
    file:delete("something.db", []).

adapter_open_close_works() ->
    AfterCloseGoal = #{file => "schemata_tests.db",
                       dbname => anonymous,
                       adapter => sqlite_adapter},

    ConnectionInfo     = #{dbname => anonymous, file => "schemata_tests.db"},
    {ok, NewConnInfo}  = schemata:connect(ConnectionInfo, sqlite_adapter),
    {ok, ClosedConn }  = schemata:close(NewConnInfo),
    ?assert(ClosedConn == AfterCloseGoal).

simple_migrate_works() ->
    ConnectionInfo     = #{dbname => anonymous, file => "schemata_tests.db"},
    {ok, Conn}  = schemata:connect(ConnectionInfo, sqlite_adapter),
    Result = schemata:migrate(Conn).

my_migrator_basic_test_() ->
    {setup,
     fun top_setup/0,
     fun top_cleanup/1,
     [
      fun simple_migrate_works/0,
      fun adapter_open_close_works/0
     ]}.
