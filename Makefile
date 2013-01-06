# Last Change: 2013-01-05 16:27:47

ERL=erl
REBAR=./rebar

.PHONY: deps get-deps

all:
	@$(REBAR) get-deps
	@$(REBAR) compile

aque_db:
	@$(REBAR) compile skip_deps=true

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

deps:
	@$(REBAR) compile

test: all
	(erlc -o ebin test/test_aque_db.erl; erl -pa ebin deps/*/ebin -name aque_db_test@127.0.0.1 -noinput -eval "test_aque_db:start(), halt(0)")


