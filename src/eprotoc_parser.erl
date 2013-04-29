-module(eprotoc_parser).

-export([
         generate_parser/0,
         generate_code/1,
         generate_field/5,
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
    peel(Package, PackageName, []).

%% @doc MFs refers to messages and fields
peel({[], []}, _, Acc) -> Acc;
peel({[{enum, Enum, Fields}|Enums], MFs}, Level, Acc) ->
    EnumName = atom_to_name(Enum),
    Text = generate_enum(EnumName, Fields, ""),
    Acc1 = [{Level, Text}|Acc],
    peel({Enums, MFs}, Level, Acc1);
peel({[], [{message, Message, {Enums, Fields}}|MFs]}, Level, Acc) ->
    MessageName = atom_to_name(Message),
    Level1 = Level ++ "__" ++ MessageName,
    Acc1 = peel({Enums, Fields}, Level1, Acc),
    peel({[], MFs}, Level, Acc1);
peel({[], [{field, Rule, Type, AtomName, Num, Opts}|MFs]}, Level, Acc) ->
    Name = atom_to_list(AtomName),
    Text = generate_field(Rule, Type, Name, Num, Opts),
    Acc1 = [{Level, Text}|Acc],
    peel({[], MFs}, Level, Acc1).

%% @doc Supply getter and setter functions for the specified field
generate_field(Rule, Type, Name, Num, Opts) ->
    NumString = integer_to_list(Num),
    Default = proplists:get_value(default, Opts),
    DefaultString = case Default of
                        D when is_atom(D) ->
                            atom_to_list(Default);
                        D when is_integer(D) ->
                            integer_to_list(D);
                        D when is_list(D) ->
                            "\"" ++ D ++ "\"";
                        D when is_float(D) ->
                            float_to_list(D)
                    end,
    "g_" ++ Name ++ "(Data) ->\n"
        "    case lists:keysearch(" ++ NumString ++ ", 1, Data) of\n"
        "        {value, {_, Value}} -> Value;\n"
        "        false -> " ++ DefaultString ++ "\n"
        "    end.\n"
        "s_" ++ Name ++ "(Data, Value) ->\n"
        "    lists:keystore(" ++ NumString ++ ", 1, Data, {" ++ NumString ++ ", Value}).\n".

generate_enum(Name, [{FieldAtom, Value}], Acc) ->
    Field = atom_to_name(FieldAtom),
    Acc ++ Name ++ "(" ++ Field ++ ") -> " ++ Value ++ ".\n";
generate_enum(Name, [{FieldAtom, Value}|Fields], Acc) ->
    Field = atom_to_name(FieldAtom),
    Acc1 = Acc ++ Name ++ "(" ++ Field ++ ") -> " ++ Value ++ ";\n",
    generate_enum(Name, Fields, Acc1).

generate_messages(Messages) ->
    lists:map(fun({message, Message, {Enums, Fields}}) ->
                      MessageName = atom_to_name(Message),
                      generate_message(MessageName, Enums, Fields, [])
              end, Messages).

generate_message(Name, [Enum|Enums], Fields, Acc) ->
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
