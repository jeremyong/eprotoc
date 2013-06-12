.PHONY: all test

all:
	rebar compile
	dialyzer ebin/

clean:
	rm -rf ebin .eunit

test: all
	./eprotoc test/test.proto test
	rebar eunit
