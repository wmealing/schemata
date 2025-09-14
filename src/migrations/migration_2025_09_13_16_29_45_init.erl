-module(migration_2025_09_13_16_29_45_init).
-export([id/0 ,up/1, down/1]).

id() ->
    "003".

up(_Conn) ->
    "CREATE TABLE event ( \
       creation_time DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL, \
       event_name TEXT NOT NULL, \
       event_data TEXT, \
       deleted BOOLEAN DEFAULT FALSE \
   );".

down(_Conn) ->
    "DROP TABLE event;". 

