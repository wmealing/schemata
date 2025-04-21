-module(sqlite_adapter_tests).

-include_lib("eunit/include/eunit.hrl").

top_setup() ->
    file:delete("something.db", []).


top_cleanup(_) ->
    file:delete("something.db", []).

sqlite_connect_makes_file() ->
    {ok, S} = sqlite_adapter:connect(something),
    ?assert(filelib:is_file("something.db") == true).

sqlite_close_doesnt_delete_files() ->
    Conn = sqlite_adapter:connect(something),
    sqlite_adapter:close(Conn),
    ?assert(filelib:is_file("./something.db") == true).


sqlite_connect_test_() ->
    {setup,
     fun top_setup/0,
     fun top_cleanup/1,
     [fun sqlite_connect_makes_file/0]}.
