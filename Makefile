.PHONY: all test

all:
	rebar compile

clean:
	rm -rf ebin .eunit

test: all
	./eprotoc test/test.proto test test
	dialyzer ebin
	rebar eunit
