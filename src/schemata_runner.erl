%%% File: src/schemata_runner.erl
-module(schemata_runner).
-export([apply_pending/2, rollback_last/2]).

-include_lib("eunit/include/eunit.hrl").

apply_pending(Conn, Adapter) ->
    Pid = maps:get(pid, Conn),
    schemata:ensure_tracking_table(Conn, Adapter),
    Applied = schemata:applied_migrations(Pid, Adapter),
    All = migration_modules(),
    lists:foreach(
      fun(Mod) ->
          Id = Mod:id(),
          case lists:member(Id, Applied) of
              true ->
                  io:format("Migration ID ~s already in database, not running ~n", [Id]),  
                  ok;
              false ->
                  io:format("Migration ID ~s not found in database, running migration~n", [Id]),
                  Sql = Mod:up(Pid),
                  Adapter:exec(Pid, Sql),
                  schemata:record_migration(Pid, Id, Adapter)
          end
      end, All).

rollback_last(Conn, Adapter) ->
    Applied = schemata:applied_migrations(Conn, Adapter),
    case lists:reverse(Applied) of
        [Last|_] ->
            Mod = migration_module_by_id(Last),
            Sql = Mod:down(Conn),
            Adapter:exec(Conn, Sql),
            schemata:remove_migration(Conn, Last, Adapter);
        [] ->
            io:format("No migrations to rollback~n"),
            ok
    end.

migration_modules() ->
    {ok, Files} = file:list_dir("src/migrations"),
    lists:flatmap(
        fun(F) ->
            _ModStr = filename:rootname(F),
            FullPath = filename:join("src/migrations", F),
            {ok, Mod } = compile:file(FullPath, {outdir, code:lib_dir(schemata) ++ "/ebin"}),

            %% oddly, i need to run this first, or it wont recognise
            %% the module as current.
            Mod:module_info(),

            case erlang:function_exported(Mod, id, 0) of
                true ->
                      [Mod];
                false ->
                      []
            end
        end, Files).

migration_module_by_id(Id) ->
    Mods = migration_modules(),
    case lists:filter(fun(Mod) -> Mod:id() =:= Id end, Mods) of
        [Mod|_] -> Mod;
        [] -> exit({migration_not_found, Id})
    end.

