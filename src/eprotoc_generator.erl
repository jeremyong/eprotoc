-module(eprotoc_generator).

-export([
         generate_parser/0,
         process_file/3,
         read_and_scan/1,
         generate_code/1,
         output_results/2,
         parse_file/3,
         reserved_words/1,
         atom_to_name/1
        ]).

generate_parser() ->
    yecc:file("src/proto_grammar.yrl").

%% @doc Processes a proto file in four steps. First, it parses the proto file
%% and generates the code. Next, it deletes existing modules that have been generated
%% before. Last, it outputs the results of the generated code to the supplied directory.
-spec process_file(list(), list(), list()) -> ok.
process_file(File, Outdir, ImportDirs) ->
    Proto = parse_file(File, Outdir, ImportDirs),
    Result = generate_code(Proto),
    delete_existing_files(Outdir, Result),
    output_results(Outdir, Result).

find_file(_, []) ->
    error;
find_file(I, [Path | ImportDirs]) ->
    File = Path ++ "/" ++ I,
    case filelib:is_file(File) of
        true ->
            File;
        false ->
            find_file(I, ImportDirs)
    end.

%% Adds a tokenized version of the imported files into the parsed file and
%% generates code for the imports.
-spec handle_imports(tuple(), list(), list()) -> tuple().
handle_imports({Name, Imports, L}, Outdir, ImportDirs) ->
    Imported = 
        lists:foldl(fun(Import, Acc) ->
                            case find_file(Import, ImportDirs) of
                                error ->
                                    throw(Import ++ " not found");
                                File ->
                                    {ImportName, Imports1, L1} = parse_file(File, Outdir, ImportDirs),
                                    case [ImportName == I || {I, _, _} <- Acc] of
                                        [] ->
                                            Result = generate_code({ImportName, Imports1, L1}),
                                            delete_existing_files(Outdir, Result),
                                            output_results(Outdir, Result),
                                            Acc ++ [{ImportName, [], L1}| Imports1];
                                        _ ->
                                            Acc ++ Imports1
                                    end
                            end
                    end, [], Imports),
    {Name, Imported, L}.

-spec generate_code(tuple()) -> list().
generate_code(Proto) ->
    PackageName = atom_to_name(element(1, Proto)),
    Package = element(3, Proto),
    {_, Result} = peel(Package, {PackageName, [], []}, [], Proto),
    Result.

-spec parse_file(list(), list(), list()) -> tuple().
parse_file(File, Outdir, ImportDirs) ->
    Tokens = read_and_scan(File),
    {ok, Proto} = proto_grammar:parse(Tokens),
    handle_imports(Proto, Outdir, ImportDirs).

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
peel({[], []}, NestingAcc, Acc, _) -> {NestingAcc, Acc};
peel([], NestingAcc, Acc, _) -> {NestingAcc, Acc};
%% Peel all enums first
peel({[{enum, Enum, Fields}|Enums], MFs},
     {Level, AccEnums, AccMessages}, Acc, Proto) ->
    EnumName = atom_to_name(Enum),
    Text = generate_enum(EnumName, Fields, ""),
    Acc1 = [{Level, Text}|Acc],
    %% Make this enum available to everything at this nesting level or deeper.
    AccEnums1 = [{Level, Enum}|AccEnums],
    peel({Enums, MFs}, {Level, AccEnums1, AccMessages}, Acc1, Proto);
%% Because we parse a message immediately, we need to look ahead to find messages
%% at the same nesting level. We do this right after parsing the enums.
peel({[], MFs}, {Level, AccEnums, AccMessages}, Acc, Proto) ->
    Messages = lists:filter(fun(MF) ->
                                    element(1, MF) == message
                            end, MFs),
    Messages1 = lists:map(fun(Msg) ->
                                  MessageAtom = element(2, Msg),
                                  MessageName = atom_to_name(MessageAtom),
                                  {Level ++ "__" ++ MessageName, MessageAtom}
                          end, Messages),
    AccMessages1 = Messages1 ++ AccMessages,
    peel(MFs, {Level, AccEnums, AccMessages1}, Acc, Proto);
peel([{message, Message, {Enums, Fields}}|MFs],
     {Level, AccEnums, AccMessages}, Acc, Proto) ->
    MessageName = atom_to_name(Message),
    Level1 = Level ++ "__" ++ MessageName,
    %% Generate message encoding and decoding functions with everything available
    %% at this level of nesting thus far.
    {{_, AccEnums1, AccMessages1}, Acc1} =
        peel({Enums, Fields}, {Level1, AccEnums, AccMessages}, Acc, Proto),
    Text = generate_message(Fields, AccEnums1, AccMessages1, Proto),
    %% Make this message available to everything at this nesting level or deeper.
    %% Note that the messages added to the scope when parsing the message contents
    %% are thrown away!
    AccMessages2 = [{Level1, Message}|AccMessages],
    peel(MFs, {Level, AccEnums, AccMessages2}, [{Level1, Text}|Acc1], Proto);
peel([{field, Rule, Type, AtomName, Num, Opts}|MFs],
     {Level, AccEnums, AccMessages}, Acc, Proto) ->
    Name = atom_to_list(AtomName),
    Text = generate_field(Rule, Name, Num, Opts, Type),
    Acc1 = [{Level, Text}|Acc],
    peel(MFs, {Level, AccEnums, AccMessages}, Acc1, Proto).

%% @doc Determine the corresponding Erlang type for each .proto type.
erlang_type(int32) -> "integer()";
erlang_type(uint32) -> "integer()";
erlang_type(sint32) -> "integer()";
erlang_type(int64) -> "integer()";
erlang_type(uint64) -> "integer()";
erlang_type(sint64) -> "integer()";
erlang_type(bool) -> "boolean()";
erlang_type(string) -> "binary()";
erlang_type(bytes) -> "binary()";
erlang_type(_) -> "term()".

%% @doc Supply getter and setter functions for the specified field
generate_field(Rule, Name, Num, Opts, Type) ->
    N = integer_to_list(Num),
    Default = proplists:get_value(default, Opts),
    DefaultString = case Default of
                        undefined when Rule =:= repeated ->
                            "[]";
                        D when is_atom(D) ->
                            atom_to_name(D);
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
    ValueType = case {Rule, erlang_type(Type)} of
                    {repeated, T} -> "list(" ++ T ++ ")";
                    {_, T} -> T
                end,
    ReturnType = case Default of
                     undefined when Rule /= repeated ->
                         ValueType ++ " | undefined";
                     _ ->
                         ValueType
                 end,
    Getter = "-spec g_" ++ Name ++ "(Data :: list()) -> " ++ ReturnType ++ ".\n"
        "g_" ++ Name ++ "(Data) ->\n"
        "    case lists:keysearch(" ++ Name ++ ", 1, Data) of\n"
        "        {value, {_, {_, _, " ++ VString ++ "}}} -> Value;\n"
        "        false ->\n"
        "            " ++ DefaultString ++ "\n"
        "    end.\n",
    Setter = "-spec s_" ++ Name ++ "(Data :: list(), Value :: " ++ ValueType ++ ") -> list().\n"
        "s_" ++ Name ++ "(Data, Value) ->\n"
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

generate_message(Fields, Enums, Messages, Proto) ->
    FieldsOnly = lists:filter(fun(Elem) -> element(1, Elem) == field end, Fields),
    {FieldRules, RuleString} = generate_message_rules(FieldsOnly, {[],""}),
    GenGets = generate_gen_gets(FieldsOnly, ""),
    GenSets = generate_gen_sets(FieldsOnly, ""),
    FieldRulesString = io_lib:format("~w", [FieldRules]),
    generate_message_lookups(FieldsOnly, "") ++
        RuleString ++
        generate_message_types(FieldsOnly, "", Enums, Messages, Proto) ++
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
        "-spec encode(Data :: list()) -> iolist() | {error, Reason :: term()}.\n"
        "encode(Data) ->\n"
        "    FieldRules = " ++ FieldRulesString ++ ",\n"
        "    case eprotoc:check_fields(Data, FieldRules) of\n"
        "        ok -> \n"
        "            eprotoc:encode_message(Data);\n"
        "        {error, E} ->\n"
        "            {error, E}\n"
        "    end.\n\n" ++
        GenGets ++ GenSets ++
        "-spec mget(Data :: list(), Fields :: list(atom())) -> list().\n"
        "mget(Data, Fields) ->\n"
        "    [get(Data,Field) || Field <- Fields].\n\n"
        "-spec mset(Data :: list(), L :: list(tuple())) -> list().\n"
        "mset(Data, L) ->\n"
        "    lists:foldl(fun({Field, Value}, Acc) -> set(Acc, Field, Value) end, Data, L).\n\n".

generate_gen_gets([], _) -> "";
generate_gen_gets([{field,_,_,FieldAtom,_,_}],Acc) ->
    Name = atom_to_name(FieldAtom),
    Acc ++ "get(Data, " ++ Name ++ ") -> g_" ++ Name ++ "(Data).\n\n";
generate_gen_gets([{field,_,_,FieldAtom,_,_}|Rest],Acc) ->
    Name = atom_to_name(FieldAtom),
    Acc1 = Acc ++ "get(Data, " ++ Name ++ ") -> g_" ++ Name ++ "(Data);\n",
    generate_gen_gets(Rest, Acc1).

generate_gen_sets([], _) -> "";
generate_gen_sets([{field,_,_,FieldAtom,_,_}],Acc) ->
    Name = atom_to_name(FieldAtom),
    Acc ++ "set(Data, " ++ Name ++ ", Value) -> s_" ++ Name ++ "(Data, Value).\n\n";
generate_gen_sets([{field,_,_,FieldAtom,_,_}|Rest],Acc) ->
    Name = atom_to_name(FieldAtom),
    Acc1 = Acc ++ "set(Data, " ++ Name ++ ", Value) -> s_" ++ Name ++ "(Data, Value);\n",
    generate_gen_sets(Rest, Acc1).


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
generate_message_rules([], _) -> {[],""};
generate_message_rules([{field, Rule, _, FieldAtom, _, _}], {L,Acc}) ->
    Name = atom_to_name(FieldAtom),
    {[{FieldAtom,Rule}|L],Acc ++ "get_rule(" ++ Name ++ ") -> " ++ atom_to_list(Rule) ++ ".\n\n"};
generate_message_rules([{field, Rule, _, FieldAtom, _, _}|Rest], {L,Acc}) ->
    Name = atom_to_name(FieldAtom),
    Acc1 = Acc ++ "get_rule(" ++ Name ++ ") -> " ++ atom_to_list(Rule) ++ ";\n",
    generate_message_rules(Rest, {[{FieldAtom,Rule}|L], Acc1}).

%% No fields in message, nothing to do.
generate_message_types([], _, _, _, _) -> "";
generate_message_types([{field, _, Type, FieldAtom, _, _}],
                       Acc, Enums, Messages, Proto) ->
    Name = atom_to_name(FieldAtom),
    Acc ++ "get_type(" ++ Name ++ ") -> " ++
        get_field_type(Type, Enums, Messages, Proto) ++ ".\n\n";
generate_message_types([{field, _, Type, FieldAtom, _, _}|Rest],
                       Acc, Enums, Messages, Proto) ->
    Name = atom_to_name(FieldAtom),
    Acc1 = Acc ++ "get_type(" ++ Name ++ ") -> " ++
        get_field_type(Type, Enums, Messages, Proto) ++ ";\n",
    generate_message_types(Rest, Acc1, Enums, Messages, Proto).

%% Search the messages of parsed proto file for the nested Enum definition.
-spec enum_search(term(), list(), list()) -> {list(), list()} | false.
enum_search({nested, Inside, Type}, Msgs, Acc) ->
    case lists:keyfind(Inside, 2, Msgs) of
        {message, _, {Enums, Msgs1}} ->
            case Type of
                {nested, _, _} ->
                    enum_search(Type, Msgs1, Acc ++ "__" ++ atom_to_name(Inside));
                _ ->
                    enum_search(Type, Enums, Acc ++ "__" ++ atom_to_name(Inside))
            end;
        _ ->
            false
    end;
enum_search(Type, Enums, Acc) ->
    case lists:keyfind(Type, 2, Enums) of
        {enum, _, _} ->
            {Acc, atom_to_name(Type)};
        _ ->
            false
    end.

%% Search the messages of parsed proto file for the nested Message definition.
-spec message_search(term(), list(), list()) -> list() | false.
message_search({nested, Inside, Type}, Msgs, Acc) ->
    case lists:keyfind(Inside, 2, Msgs) of
        {message, _, {_, Msgs1}} ->
            message_search(Type, Msgs1, Acc ++ atom_to_name(Inside) ++ "__");
        _ ->
            false
    end;
message_search(Type, Msgs, Acc) ->
    case lists:keyfind(Type, 2, Msgs) of
        {message, _, _} ->
            Acc ++ atom_to_name(Type);
        _ ->
            false
    end.

nested_atom_to_name({nested, Inside, Type}, Acc) ->
    nested_atom_to_name(Type,Acc ++ atom_to_name(Inside) ++ "__");
nested_atom_to_name(Type, Acc) ->
    Acc ++ atom_to_name(Type).

get_field_type({nested, Inside, Type}, _, _, Proto) ->
    case lists:keysearch(Inside, 1, element(2, Proto)) of
        false ->
            PackageName = atom_to_name(element(1, Proto)),
            Msgs = element(2, element(3, Proto)),
            case {message_search({nested, Inside, Type}, Msgs, PackageName ++ "__"),
                  enum_search({nested, Inside, Type}, Msgs, PackageName)} of
                {false, false} ->
                    throw("Nested message type or enum " ++ 
                              nested_atom_to_name({nested, Inside, Type}, "") ++ " not found.");
                {Level, false} ->
                    "{message, fun " ++ Level ++ ":decode/1, fun " ++ Level ++ ":encode/1}";
                {false, {Level, Enum}} ->
                    "{enum, fun " ++ Level ++ ":" ++ Enum ++ "_enum/1}";
                {_, _} ->
                    throw("Ambiguous message or enum type " ++ Type ++ ".")
            end;
        {value, Proto1} ->
            case Type of
                %% Start searching the imported file for the field type.
                {nested, _, _} ->
                    get_field_type(Type, [], [], Proto1);
                %% Special case. Message is at top level of imported file.
                _ ->
                    case lists:keyfind(Type, 2, element(2, element(3, Proto1))) of
                        false ->
                            throw("Imported message " ++ atom_to_name(Type) ++ " not found.");
                        _ ->
                            Level = atom_to_name(Inside) ++ "__" ++ atom_to_name(Type),
                            "{message, fun " ++ Level ++ ":decode/1, fun " ++ Level ++ ":encode/1}"
                    end
            end
    end;
get_field_type(TypeAtom, Enums, Messages, _) ->
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
reserved_words(import) -> true;
reserved_words(_) -> false.

atom_to_name(Atom) ->
    PascalCaseName = atom_to_list(Atom),
    UnderScoreName = re:replace(PascalCaseName,
                                "(.+)([A-Z])", "\\1_\\2",
                                [ungreedy, global, {return, list}]),
    string:to_lower(UnderScoreName).
