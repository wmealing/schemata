#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname new-migration 

main([String]) ->
    try
        io:format("making file ~s ", [String])
    catch
        _:_ ->
            usage()
    end;

main(_) ->
    usage().

usage() ->
    io:format("usage: new-migration migration-name\n"),
    halt(1).

