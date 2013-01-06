AQUEDB: A simple key-value DB abstraction for web game
===========================================================
Inspired by boss_db https://github.com/evanmiller/boss_db

Supported databases
-------------------
* Mnesia
* MongoDB
* MySQL

Usage
-----
    application:start(aque_db).

    OR
    
    aque_db:start().
    
    Example of DBOptions:  % in aque_db.app.src
        DBOptions = 
        [
            [
                {adapter, {local, mnesia|mysql|mongodb},
                {db_host, HostName::string()},
                {db_port, PortNumber::integer()},
                {db_username, UserName::string()},
                {db_password, Password::string()},
                {db_database, Database::string()}
            ]
        ]


Introduction
------------
    A simple KEY-VALUE DB abstraction for web game.
    Do not use AqueDB, if you need update data in DB client direct.

    The type(integer, string or other) of KEY depends on the design of your table.
    The VALUE can be any erlang term, it will be converted to binary by term_to_binary/1 before inserted into databases.

    AqueDb provides:
        
        init tables(see aque_db.app.src)

        pool(poolboy) 

        operations: 
            insert
            delete
            lookup
            all_keys
            count
            counter
            update_counter 
            tab2list

    If you use mnesia, don't forget to create schema before start AqueDb.

