.PHONY: all test

all:
	erlc +debug_info -o ebin -pa ebin src/*.erl

clean:
	rm -rf ebin .eunit

test:
	./eprotoc test/test.proto test
	rebar eunit
