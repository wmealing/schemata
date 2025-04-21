%%% File: src/my_migrator_runner.erl
-module(my_migrator_runner).
-export([apply_pending/2, rollback_last/2]).

apply_pending(Conn, Adapter) ->
    my_migrator_repo:ensure_tracking_table(Conn, Adapter),
    Applied = my_migrator_repo:applied_migrations(Conn, Adapter),
    All = migration_modules(),
    lists:foreach(
      fun(Mod) ->
          Id = Mod:id(),
          case lists:member(Id, Applied) of
              true -> ok;
              false ->
                  Sql = Mod:up(Conn),
                  Adapter:exec(Conn, Sql),
                  my_migrator_repo:record_migration(Conn, Id, Adapter)
          end
      end, All).

rollback_last(Conn, Adapter) ->
    Applied = my_migrator_repo:applied_migrations(Conn, Adapter),
    case lists:reverse(Applied) of
        [Last|_] ->
            Mod = migration_module_by_id(Last),
            Sql = Mod:down(Conn),
            Adapter:exec(Conn, Sql),
            my_migrator_repo:remove_migration(Conn, Last, Adapter);
        [] ->
            io:format("No migrations to rollback~n"),
            ok
    end.

migration_modules() ->
    {ok, Files} = file:list_dir("src/migrations"),
    Erls = [F || F <- Files,
                  filename:extension(F) =:= ".erl",
                  case filename:basename(F) of
                      ["m" | _] -> true;
                      _ -> false
                  end],
    lists:flatmap(
        fun(F) ->
            _ModStr = filename:rootname(F),
            FullPath = filename:join("src/migrations", F),
            {ok, {module, Mod}} = compile:file(FullPath, [report, return]),
            case erlang:function_exported(Mod, id, 0) of
                true -> [Mod];
                false -> []
            end
        end, Erls).

migration_module_by_id(Id) ->
    Mods = migration_modules(),
    case lists:filter(fun(Mod) -> Mod:id() =:= Id end, Mods) of
        [Mod|_] -> Mod;
        [] -> exit({migration_not_found, Id})
    end.

