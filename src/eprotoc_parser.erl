-module(eprotoc_parser).

-export([
         generate_parser/0,
         generate_code/1,
         parse_file/1,
         reserved_words/1,
         atom_to_name/1
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
    PackageName = atom_to_name(element(1, Proto)),
    Package = element(2, Proto),
    generate_enums(element(1, Package)).

generate_enums(Enums) ->
    lists:map(fun({enum, Enum, Fields}) ->
                      EnumName = atom_to_name(Enum),
                      generate_enum(EnumName, Fields, "")
              end, Enums).

generate_enum(Name, [{FieldAtom, Value}], Acc) ->
    Field = atom_to_name(FieldAtom),
    Acc ++ Name ++ "(" ++ Field ++ ") -> " ++ Value ++ ".";
generate_enum(Name, [{FieldAtom, Value}|Fields], Acc) ->
    Field = atom_to_name(FieldAtom),
    Acc1 = Acc ++ Name ++ "(" ++ Field ++ ") -> " ++ Value ++ ";\n",
    generate_enum(Name, Fields, Acc1).

generate_messages(Messages) ->
    ok.

reserved_words(package) -> true;
reserved_words(message) -> true;
reserved_words(enum) -> true;
reserved_words(packed) -> true;
reserved_words(default) -> true;
reserved_words(_) -> false.

atom_to_name(Atom) ->
    PascalCaseName = atom_to_list(Atom),
    UnderScoreName = re:replace(PascalCaseName,
                                "(.+)([A-Z])", "\\1_\\2",
                                [ungreedy, global, {return, list}]),
    string:to_lower(UnderScoreName).
