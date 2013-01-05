AQUEDB: A simple key-value DB abstraction for webgame or others
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
    A simple key-value DB abstraction for webgame or others.
    Most of data is convert to binary by term_to_binary/1 before inserted into databases.
    Do not use AqueDB, if you need update data in DB client direct.

    AqueDb provides:
        
        init tables(see aque_db.app.src)

        pool(poolboy) 

        AND

        operations: 
            insert
            delete
            lookup
            all_keys
            count
            counter
            update_counter 
            tab2list

    If you use mnesia, don't forget create schema before start AqueDb.

