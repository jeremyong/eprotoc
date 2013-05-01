-module(eprotoc_generator).

-export([
         generate_parser/0,
         process_file/2,
         read_and_scan/1,
         generate_code/1,
         output_results/2,
         parse_file/1,
         reserved_words/1,
         atom_to_name/1
        ]).

generate_parser() ->
    yecc:file("src/proto_grammar.yrl").

%% @doc Processes a proto file in three steps. First, it parses the proto file
%% and generates the code. Next, it deletes existing modules that have been generated
%% before. Last, it outputs the results of the generated code to the supplied directory.
-spec process_file(list(), list()) -> ok.
process_file(File, Outdir) ->
    Result = generate_code(File),
    delete_existing_files(Outdir, Result),
    output_results(Outdir, Result).

-spec generate_code(list()) -> list().
generate_code(File) ->
    {ok, Proto} = parse_file(File),
    PackageName = atom_to_name(element(1, Proto)),
    Package = element(2, Proto),
    {_, Result} = peel(Package, {PackageName, [], []}, []),
    Result.

-spec parse_file(list()) -> list().
parse_file(File) ->
    Tokens = read_and_scan(File),
    proto_grammar:parse(Tokens).

%% @doc Reads a file into memory and scans it, deleting comments along the way.
-spec read_and_scan(list()) -> list().
read_and_scan(File) ->
    {ok, Contents} = file:read_file(File),
    StripComments = re:replace(Contents, "/\\*([^*]|\\*+[^*/])*\\*+/", "",
                               [global, {return, list}]),
    StripComments1 = re:replace(StripComments, "//.*\\n", "",
                                [global, {return, list}]),
    {ok, Tokens, _} = erl_scan:string(StripComments1, 1,
                        {reserved_word_fun, fun reserved_words/1}
                       ),
    Tokens.

delete_existing_files(Dir, [{Module, _}|Rest]) ->
    Filepath = get_filepath(Dir, Module),
    case filelib:is_file(Filepath) of
        false ->
            ok;
        true ->
            file:delete(Filepath)
    end,
    delete_existing_files(Dir, Rest);
delete_existing_files(_, []) ->
    ok.

output_results(Dir, []) ->
    io:format("Output finished to directory ~p~n", [Dir]),
    ok;
output_results(Dir, [{Module,Text}|Rest]) ->
    Filepath = get_filepath(Dir, Module),
    case filelib:is_file(Filepath) of
        false ->
            file:write_file(Filepath,
                            "-module(" ++ Module ++ ").\n"
                            "-compile(export_all).\n\n");
        true ->
            ok
    end,
    file:write_file(Filepath, Text, [append]),
    output_results(Dir, Rest).

%% hidden
get_filepath(Dir, File) ->
    Dir ++ "/" ++ File ++ ".erl".

%% @doc Hidden. MFs refers to messages or fields (they may be at the same nesting level.
%% The second argument is a nesting accumulator of the form:
%% {nesting level, enums available at this scope, messages available at this scope}
%% The nesting level doubles as the module name of the current scope
%% This code essentially functions like an onion (albeit a complicated one),
%% hence the name.
peel({[], []}, NestingAcc, Acc) -> {NestingAcc, Acc};
peel([], NestingAcc, Acc) -> {NestingAcc, Acc};
%% Peel all enums first
peel({[{enum, Enum, Fields}|Enums], MFs},
     {Level, AccEnums, AccMessages}, Acc) ->
    EnumName = atom_to_name(Enum),
    Text = generate_enum(EnumName, Fields, ""),
    Acc1 = [{Level, Text}|Acc],
    %% Make this enum available to everything at this nesting level or deeper.
    AccEnums1 = [{Level, Enum}|AccEnums],
    peel({Enums, MFs}, {Level, AccEnums1, AccMessages}, Acc1);
%% Because we parse a message immediately, we need to look ahead to find messages
%% at the same nesting level. We do this right after parsing the enums.
peel({[], MFs}, {Level, AccEnums, AccMessages}, Acc) ->
    Messages = lists:filter(fun(MF) ->
                                    element(1, MF) == message
                            end, MFs),
    Messages1 = lists:map(fun(Msg) ->
                                  MessageAtom = element(2, Msg),
                                  MessageName = atom_to_name(MessageAtom),
                                  {Level ++ "__" ++ MessageName, MessageAtom}
                          end, Messages),
    AccMessages1 = Messages1 ++ AccMessages,
    peel(MFs, {Level, AccEnums, AccMessages1}, Acc);
peel([{message, Message, {Enums, Fields}}|MFs],
     {Level, AccEnums, AccMessages}, Acc) ->
    MessageName = atom_to_name(Message),
    Level1 = Level ++ "__" ++ MessageName,
    %% Generate message encoding and decoding functions with everything available
    %% at this level of nesting thus far.
    {{_, AccEnums1, AccMessages1}, Acc1} =
        peel({Enums, Fields}, {Level1, AccEnums, AccMessages}, Acc),
    Text = generate_message(Fields, AccEnums1, AccMessages1),
    %% Make this message available to everything at this nesting level or deeper.
    %% Note that the messages added to the scope when parsing the message contents
    %% are thrown away!
    AccMessages2 = [{Level1, Message}|AccMessages],
    peel(MFs, {Level, AccEnums, AccMessages2}, [{Level1, Text}|Acc1]);
peel([{field, Rule, _Type, AtomName, Num, Opts}|MFs],
     {Level, AccEnums, AccMessages}, Acc) ->
    Name = atom_to_list(AtomName),
    Text = generate_field(Rule, Name, Num, Opts),
    Acc1 = [{Level, Text}|Acc],
    peel(MFs, {Level, AccEnums, AccMessages}, Acc1).

%% @doc Supply getter and setter functions for the specified field
generate_field(Rule, Name, Num, Opts) ->
    N = integer_to_list(Num),
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
    %% If no default is assigned, return undefined if unset.
    %% If a default is assigned and the key is not found, set the key to the
    %% default value and return the default value.
    {F, B} = case Rule of
                 repeated -> {"{repeated, ", "}"};
                 _ -> {"", ""}
             end,
    VString = F ++ "Value" ++ B,
    Getter = case Default of
                 undefined ->
                     "g_" ++ Name ++ "(Data) ->\n"
                         "    case lists:keysearch(" ++ Name ++ ", 1, Data) of\n"
                         "        {value, {_, {_, _, " ++ VString ++ "}}} -> " ++ VString ++ ";\n"
                         "        false -> undefined\n"
                         "    end.\n";
                 _ ->
                     "g_" ++ Name ++ "(Data) ->\n"
                         "    case lists:keysearch(" ++ Name ++ ", 1, Data) of\n"
                         "        {value, {_, {_, _, " ++ VString ++ "}}} -> " ++ VString ++ ";\n"
                         "        false ->\n"
                         "            s_" ++ Name ++ "(Data, " ++ F ++ DefaultString ++ B ++ "),\n"
                         "            " ++ F ++ DefaultString ++ B ++ "\n"
                         "    end.\n"
             end,
    Setter = "s_" ++ Name ++ "(Data, Value) ->\n"
        "    Type = case get_type(" ++ Name ++ ") of\n"
        "               {message, _, Encode} -> {message, Encode};\n"
        "               T -> T\n"
        "           end,\n"
        "    lists:keystore(" ++ Name ++ ", 1, Data, {" ++ Name ++ ", {" ++ N ++
        ", Type, " ++ VString ++ "}}).\n\n",
    Getter ++ Setter.

%% @doc Generates forward and reverse lookups for simplicity.
generate_enum(Name, [{FieldAtom, Value}], Acc) ->
    Field = atom_to_name(FieldAtom),
    Acc1 = Acc ++ Name ++ "_enum(" ++ Field ++ ") -> " ++ Value ++ ";\n",
    Acc2 = Acc1 ++ Name ++ "_enum(" ++ Value ++ ") -> " ++ Field ++ ";\n",
    Acc2 ++ Name ++ "_enum(_) -> undefined.\n\n";
generate_enum(Name, [{FieldAtom, Value}|Fields], Acc) ->
    Field = atom_to_name(FieldAtom),
    Acc1 = Acc ++ Name ++ "_enum(" ++ Field ++ ") -> " ++ Value ++ ";\n",
    Acc2 = Acc1 ++ Name ++ "_enum(" ++ Value ++ ") -> " ++ Field ++ ";\n",
    generate_enum(Name, Fields, Acc2).

generate_message(Fields, Enums, Messages) ->
    FieldsOnly = lists:filter(fun(Elem) -> element(1, Elem) == field end, Fields),
    generate_message_lookups(FieldsOnly, "") ++
        generate_message_rules(FieldsOnly, "") ++
        generate_message_types(FieldsOnly, "", Enums, Messages) ++
        "-spec decode(Payload :: binary()) -> list().\n"
        "decode(Payload) ->\n"
        "    Raw = eprotoc:decode_message(Payload),\n"
        "    eprotoc:reverse_repeated_fields(map_values(Raw)).\n\n"
        "-spec map_values(iolist()) -> list().\n"
        "map_values(Raw) ->\n"
        "    map_values(Raw, []).\n\n"
        "map_values([], Acc) -> Acc;\n"
        "map_values([{Num, _WireType, Value}|Rest], Acc) ->\n"
        "    Field = lookup_field(Num),\n"
        "    case Field of\n"
        "        undefined -> map_values(Rest, Acc);\n"
        "        _ ->\n"
        "            Type = get_type(Field),\n"
        "            {Result, Type1} = case Type of\n"
        "                                  {message, Decode, Encode} ->\n"
        "                                      {Decode(Value), {message, Encode}};\n"
        "                                  {enum, Fun} ->\n"
        "                                      {Fun(Value), {enum, Fun}};\n"
        "                                  _ ->\n"
        "                                      {eprotoc:cast_type(Type, Value), Type}\n"
        "                              end,\n"
        "            Acc1 = case get_rule(Field) of\n"
        "                       repeated ->\n"
        "                           V = case lists:keysearch(Field, 1, Acc) of\n"
        "                                   {value, OldValue} -> OldValue;\n"
        "                                   false -> {Field, {Num, Type1, {repeated, []}}}\n"
        "                               end,\n"
        "                           {repeated, Vs} = element(3, element(2, V)),\n"
        "                           V1 = {Field, {Num, Type1, {repeated,[Result|Vs]}}},\n"
        "                           lists:keystore(Field, 1, Acc, V1);\n"
        "                       _ ->\n"
        "                           [{Field, {Num, Type1, Result}}|Acc]\n"
        "                   end,\n"
        "            map_values(Rest, Acc1)\n"
        "    end.\n\n"
        "-spec encode(Data :: list()) -> iolist().\n"
        "encode(Data) ->\n"
        "    eprotoc:encode_message(Data).\n\n".

%% No fields in message, nothing to do.
generate_message_lookups([], _) -> "";
generate_message_lookups([{field, _, _, FieldAtom, Num, _}], Acc) ->
    Name = atom_to_name(FieldAtom),
    Acc ++ "lookup_field(" ++ integer_to_list(Num) ++ ") -> " ++ Name ++ ";\n"
        "lookup_field(_) -> undefined.\n\n";
generate_message_lookups([{field, _, _, FieldAtom, Num, _}|Rest], Acc) ->
    Name = atom_to_name(FieldAtom),
    Acc1 = Acc ++ "lookup_field(" ++ integer_to_list(Num) ++ ") -> " ++ Name ++ ";\n",
    generate_message_lookups(Rest, Acc1).

%% No fields in message, nothing to do.
generate_message_rules([], _) -> "";
generate_message_rules([{field, Rule, _, FieldAtom, _, _}], Acc) ->
    Name = atom_to_name(FieldAtom),
    Acc ++ "get_rule(" ++ Name ++ ") -> " ++ atom_to_list(Rule) ++ ".\n\n";
generate_message_rules([{field, Rule, _, FieldAtom, _, _}|Rest], Acc) ->
    Name = atom_to_name(FieldAtom),
    Acc1 = Acc ++ "get_rule(" ++ Name ++ ") -> " ++ atom_to_list(Rule) ++ ";\n",
    generate_message_rules(Rest, Acc1).

%% No fields in message, nothing to do.
generate_message_types([], _, _, _) -> "";
generate_message_types([{field, _, Type, FieldAtom, _, _}],
                       Acc, Enums, Messages) ->
    Name = atom_to_name(FieldAtom),
    Acc ++ "get_type(" ++ Name ++ ") -> " ++
        get_field_type(Type, Enums, Messages) ++ ".\n\n";
generate_message_types([{field, _, Type, FieldAtom, _, _}|Rest],
                       Acc, Enums, Messages) ->
    Name = atom_to_name(FieldAtom),
    Acc1 = Acc ++ "get_type(" ++ Name ++ ") -> " ++
        get_field_type(Type, Enums, Messages) ++ ";\n",
    generate_message_types(Rest, Acc1, Enums, Messages).

get_field_type(TypeAtom, Enums, Messages) ->
    Type = atom_to_name(TypeAtom),
    case eprotoc:wire_type(TypeAtom) of
        custom ->
            case {lists:keysearch(TypeAtom, 2, Enums),
                  lists:keysearch(TypeAtom, 2, Messages)} of
                {{value, {Level, EnumAtom}}, false} ->
                    %% Enums are treated like uint32 types on the wire
                    Enum = atom_to_name(EnumAtom),
                    "{enum, fun " ++ Level ++ ":" ++ Enum ++ "_enum/1}";
                {false, {value, {Level, _}}} ->
                    %% Custom types are other messages that provide their own mapping funs.
                    "{message, fun " ++ Level ++ ":decode/1, fun " ++ Level ++ ":encode/1}";
                {false, false} ->
                    throw("Message or enum type " ++ Type ++ " not in scope.");
                {_, _} ->
                    throw("Ambiguous message or enum type " ++ Type ++ ".")
            end;
        _ ->
            Type
    end.

%% @doc Used by erl_scan
reserved_words(package) -> true;
reserved_words(message) -> true;
reserved_words(enum) -> true;
reserved_words(packed) -> true;
reserved_words(default) -> true;
reserved_words(true) -> true;
reserved_words(false) -> true;
reserved_words(_) -> false.

atom_to_name(Atom) ->
    PascalCaseName = atom_to_list(Atom),
    UnderScoreName = re:replace(PascalCaseName,
                                "(.+)([A-Z])", "\\1_\\2",
                                [ungreedy, global, {return, list}]),
    string:to_lower(UnderScoreName).
