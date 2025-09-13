#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname new-migration

-module(new_migration).

-export([main/0, main/1,usage/0]).


create_migrations_directory() ->
    MigrationsDirPath = "./src/migrations",
    case filelib:ensure_dir(MigrationsDirPath) of
        ok -> do_nothing;
        {error, Reason} ->
            io:format("Can't create migrations directory '~s': ~p~n", [MigrationsDirPath, Reason])
    end.


get_migration_filename(MigrationName) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(os:timestamp()),

    % Format the string
    io_lib:format("~4w-~2..0w-~2..0w_~2..0w-~2..0w-~2..0w_~s.erl",
                  [Year, Month, Day, Hour, Minute, Second, MigrationName]).


generate_file(TemplatePath, OutputPath, Data) ->
    case file:read_file(TemplatePath) of
        {ok, TemplateContent} ->
            % Convert binary content to a string (list of characters) for easier manipulation
            TemplateString = binary_to_list(TemplateContent),

            % Perform substitutions
            ProcessedString = replace_placeholders(TemplateString, Data),

            % Write the processed content to the new file
            case file:write_file(OutputPath, list_to_binary(ProcessedString)) of
                ok ->
                    io:format("File '~s' generated successfully from template '~s'.~n", [OutputPath, TemplatePath]);
                {error, Reason} ->
                    io:format("Error writing file '~s': ~p~n", [OutputPath, Reason])
            end;
        {error, Reason} ->
            io:format("Error reading template file '~s': ~p~n", [TemplatePath, Reason])
    end.

replace_placeholders(String, Data) ->
    % Simple replacement for demonstration. For more complex templating, consider a library.
    String1 = string:replace(String, "{{name}}", proplists:get_value(name, Data, "Guest"), all),
    string:replace(String1, "{{date}}", proplists:get_value(date, Data, "Unknown Date"), all).



main([MigrationName]) ->
        MigrationFileName = get_migration_filename(MigrationName),
        DestinationFile = "./src/migrations/" ++ MigrationFileName,

        io:format("creating migration file ~s~n", [DestinationFile]),

        % This is a bit weird, but i'm not seeing a better way to do it.
        ScriptPath = escript:script_name(),
        TemplateParentDirectory = filename:dirname (filename:dirname(ScriptPath)),
        TemplateFile = TemplateParentDirectory ++ "/templates/template.txt",
        create_migrations_directory(),
        generate_file(TemplateFile, DestinationFile, [{migration_name, MigrationFileName}, {date, "2025-09-13"}]).

main() ->
    usage().

usage() ->
    io:format("usage: new-migration migration-name\n"),
    halt(1).

