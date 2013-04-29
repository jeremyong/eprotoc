-module(eprotoc_parser).

-export([
         generate_parser/0,
         parse_file/1,
         reserved_words/1
        ]).

generate_parser() ->
    yecc:file("src/proto_grammar.yrl").

parse_file(File) ->
    {ok, Contents} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(
                        binary_to_list(Contents), 1,
                        {reserved_word_fun, fun reserved_words/1}
                       ),
    proto_grammar:parse(Tokens).

reserved_words(package) -> true;
reserved_words(message) -> true;
reserved_words(enum) -> true;
reserved_words(packed) -> true;
reserved_words(default) -> true;
reserved_words(_) -> false.
