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
    StripComments = re:replace(Contents, "/\\*([^*]|\\*+[^*/])*\\*+/", "",
                               [global, {return, list}]),
    StripComments1 = re:replace(StripComments, "//.*\\n", "",
                                [global, {return, list}]),
    {ok, Tokens, _} = erl_scan:string(StripComments1, 1,
                        {reserved_word_fun, fun reserved_words/1}
                       ),
    proto_grammar:parse(Tokens).

generate_code(File) ->
    {ok, Proto} = parse_file(File),
    ok.

reserved_words(package) -> true;
reserved_words(message) -> true;
reserved_words(enum) -> true;
reserved_words(packed) -> true;
reserved_words(default) -> true;
reserved_words(_) -> false.
