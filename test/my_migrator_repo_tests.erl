%%% @author Wade Mealing <wmealing@Wades-MacBook-Air.local>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2025 by Wade Mealing <wmealing@Wades-MacBook-Air.local>

-module(my_migrator_repo_tests).

-include_lib("eunit/include/eunit.hrl").

top_setup() ->
    file:delete("something.db", []).

top_cleanup(_) ->
    file:delete("something.db", []).

i_do_nothing() ->
    ?assert(1 == 1).

%% more fragile than i wanted, i waned this to die properly each run.
adapter_connect_works() ->
    {ok, Something }  = my_migrator_repo:connect(something, sqlite_adapter),
    my_migrator_repo:close( Something, sqlite_adapter),
    ?assert(is_pid(Something)).

adapter_close_works() ->
    {ok, Something }  = my_migrator_repo:connect(something, sqlite_adapter),
    Result = my_migrator_repo:close(Something, sqlite_adapter),
    ?assert(Result == ok).

my_migrator_basic_test_() ->
    {setup,
     fun top_setup/0,
     fun top_cleanup/1,
     [fun i_do_nothing/0,
      fun adapter_connect_works/0,
      fun adapter_close_works/0]}.
