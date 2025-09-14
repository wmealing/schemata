-module(sqlite_adapter_tests).

-include_lib("eunit/include/eunit.hrl").

-export([sqlite_connect_makes_file/0, sqlite_close_doesnt_delete_files/0]).

top_setup() ->
    file:delete("foo.db", []).

top_cleanup(_) ->
    file:delete("foo.db", []).

sqlite_connect_makes_file() ->
    {ok, ConnInfo} = sqlite_adapter:connect(#{dbname => anonymous, file => "something.db"}),
    ?assert(filelib:is_file("something.db") == true),
    sqlite_adapter:close(ConnInfo).

sqlite_close_doesnt_delete_files() ->
    {ok, ConnInfo} = sqlite_adapter:connect(#{dbname => anonymous, file => "something.db"}),
    sqlite_adapter:close(ConnInfo),
    ?assert(filelib:is_file("./something.db") == true).

sqlite_connect_test_() ->
    {setup,
     fun top_setup/0,
     fun top_cleanup/1,
     [fun sqlite_connect_makes_file/0]}.
