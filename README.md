# eprotoc

Erlang proto file parser and code generator.

## goals

The project has several goals:

1. Avoid record syntax
  - Protocol buffer messages are expected to be used by potentially
    many modules
  - Records aren't easily shared across modules
2. Preserve the flexibility of protocol buffers
  - Protocol buffers are meant to be backwards compatibile and the
    underlying data structure should not break this
  - We are using keylists for now but this may change as benchmarks
    are performed (orddicts are also an option).
3. Avoid header files
  - Header files make it harder to reason about code and break
    encapsulation intrinsic to protocol buffers
  - Prefer instead module-based organization of messages and enums

In addition, the project introduces no new dependencies (everything is
raw erlang).

## usage

To build the project, you need `rebar` in your `$PATH`, after which
you can just run `make` or `rebar compile`.

The project ships with a simple convenience script for generating code
from `.proto` files. To do so run:

```bash
eprotoc /path/to/file.proto /path/to/src/dir
```

This will generate a number of modules to the path you specify. For
example:

```proto
package Foo;

message Bar {
    required uint32 zap = 1;
}
```

will output the module `foo__bar.erl` with the functions

```erlang
fun g_zap/1,
fun s_zap/2,
fun encode/1,
fun decode/1
```

The first two functions are a getter and setter for the `zap` field
and the last two functions are an encoder and decoder for a `bar`
message list. To create a new message of type `Bar`, we would just use
an empty list since lists are the internal structure of the message:

```erlang
NewBar = foo__bar:s_zap([], 100),
foo__bar:encode(NewBar).
```

This creates an `iolist` representing a `Bar` message with `zap` set
to `100` that can then be sent over the wire.

See the `test` folder for examples of enum usage, the repeated field,
nested messages, imported messages, and more.

## known caveats

- missing tests on a number of data types and cases of more extreme
  nesting

## roadmap

1. Add more tests
