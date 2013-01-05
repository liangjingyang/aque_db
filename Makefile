# Last Change: 2013-01-05 16:27:47

ERL=erl
REBAR=./rebar

.PHONY: deps get-deps

all:
	@$(REBAR) get-deps
	@$(REBAR) compile

aque:
	@$(REBAR) compile skip_deps=true

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

deps:
	@$(REBAR) compile

test:
	@$(REBAR) skip_deps=true eunit


