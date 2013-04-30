.PHONY: all test

all:
	rebar compile

clean:
	rm -rf ebin .eunit

test:
	./eprotoc test/test.proto test
	rebar eunit
