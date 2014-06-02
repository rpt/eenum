.PHONY: compile test clean

compile:
	@ rebar compile

test:
	@ rebar eunit

clean:
	@ rebar clean
