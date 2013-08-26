-module(eprotoc_codegen).

-export([
         generate_enum/3,
         generate_field/5,
         generate_message/4
        ]).

%% @doc Determine the corresponding Erlang type for each .proto type.
erlang_type(int32)  -> "integer()";
erlang_type(uint32) -> "integer()";
erlang_type(sint32) -> "integer()";
erlang_type(int64)  -> "integer()";
erlang_type(uint64) -> "integer()";
erlang_type(sint64) -> "integer()";
erlang_type(bool)   -> "boolean()";
erlang_type(string) -> "binary()";
erlang_type(bytes)  -> "binary()";
erlang_type(_)      -> "term()".

%% @doc
%% Generates code to look up the field name or value of a pascal-case enum.
%% Example: an enum field of `StartToken = 32` would need method that turn
%% `StartToken => 32` as well as `32 => StartToken`.
%% @end
generate_enum(Name, [], Acc) ->
    Acc ++ Name ++ "_enum(_) -> undefined.\n\n";
generate_enum(Name, [{FieldAtom, Value}|Fields], Acc) ->
    Field = eprotoc_generator:atom_to_name(FieldAtom),
    Acc1 = Acc ++ Name ++ "_enum(" ++ Field ++ ") -> " ++ Value ++ ";\n",
    Acc2 = Acc1 ++ Name ++ "_enum(" ++ Value ++ ") -> " ++ Field ++ ";\n",
    generate_enum(Name, Fields, Acc2).

%% @doc
%% Generates code for a single field within a message. This particular
%% implementation generates getter and setter functions for a single field.
%% @end
generate_field(Rule, Name, Num, Opts, Type) ->
    N = integer_to_list(Num),
    Default = proplists:get_value(default, Opts),
    DefaultString = case Default of
                        undefined when Rule =:= repeated ->
                            "[]";
                        D when is_atom(D) ->
                            eprotoc_generator:atom_to_name(D);
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

%% @doc
%% Generates code for a message. This particular implementation generates common
%% code functionality that all messages share, such as the decode and encode
%% functions. If there are no fields in the message being processed, then no
%% code will be generated.
%% @end
generate_message(Fields, Enums, Messages, Proto) ->
    FieldsOnly = lists:filter(fun(Elem) -> element(1, Elem) == field end, Fields),
    generate_message_fields(FieldsOnly, Enums, Messages, Proto).

generate_message_fields([], _, _, _) ->
    "";
generate_message_fields(FieldsOnly, Enums, Messages, Proto) ->
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
    Name = eprotoc_generator:atom_to_name(FieldAtom),
    Acc ++ "get(Data, " ++ Name ++ ") -> g_" ++ Name ++ "(Data).\n\n";
generate_gen_gets([{field,_,_,FieldAtom,_,_}|Rest],Acc) ->
    Name = eprotoc_generator:atom_to_name(FieldAtom),
    Acc1 = Acc ++ "get(Data, " ++ Name ++ ") -> g_" ++ Name ++ "(Data);\n",
    generate_gen_gets(Rest, Acc1).

generate_gen_sets([], _) -> "";
generate_gen_sets([{field,_,_,FieldAtom,_,_}],Acc) ->
    Name = eprotoc_generator:atom_to_name(FieldAtom),
    Acc ++ "set(Data, " ++ Name ++ ", Value) -> s_" ++ Name ++ "(Data, Value).\n\n";
generate_gen_sets([{field,_,_,FieldAtom,_,_}|Rest],Acc) ->
    Name = eprotoc_generator:atom_to_name(FieldAtom),
    Acc1 = Acc ++ "set(Data, " ++ Name ++ ", Value) -> s_" ++ Name ++ "(Data, Value);\n",
    generate_gen_sets(Rest, Acc1).


%% No fields in message, nothing to do.
generate_message_lookups([], Acc) ->
    Acc ++ "lookup_field(_) -> undefined.\n\n";
generate_message_lookups([{field, _, _, FieldAtom, Num, _}|Rest], Acc) ->
    Name = eprotoc_generator:atom_to_name(FieldAtom),
    Acc1 = Acc ++ "lookup_field(" ++ integer_to_list(Num) ++ ") -> " ++ Name ++ ";\n",
    generate_message_lookups(Rest, Acc1).

%% No fields in message, nothing to do.
generate_message_rules([], _) -> {[],""};
generate_message_rules([{field, Rule, _, FieldAtom, _, _}], {L,Acc}) ->
    Name = eprotoc_generator:atom_to_name(FieldAtom),
    {[{FieldAtom,Rule}|L],Acc ++ "get_rule(" ++ Name ++ ") -> " ++ atom_to_list(Rule) ++ ".\n\n"};
generate_message_rules([{field, Rule, _, FieldAtom, _, _}|Rest], {L,Acc}) ->
    Name = eprotoc_generator:atom_to_name(FieldAtom),
    Acc1 = Acc ++ "get_rule(" ++ Name ++ ") -> " ++ atom_to_list(Rule) ++ ";\n",
    generate_message_rules(Rest, {[{FieldAtom,Rule}|L], Acc1}).

%% No fields in message, nothing to do.
generate_message_types([], _, _, _, _) -> "";
generate_message_types([{field, _, Type, FieldAtom, _, _}],
                       Acc, Enums, Messages, Proto) ->
    Name = eprotoc_generator:atom_to_name(FieldAtom),
    Acc ++ "get_type(" ++ Name ++ ") -> " ++
        get_field_type(Type, Enums, Messages, Proto) ++ ".\n\n";
generate_message_types([{field, _, Type, FieldAtom, _, _}|Rest],
                       Acc, Enums, Messages, Proto) ->
    Name = eprotoc_generator:atom_to_name(FieldAtom),
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
                    enum_search(Type, Msgs1, Acc ++ "__" ++ eprotoc_generator:atom_to_name(Inside));
                _ ->
                    enum_search(Type, Enums, Acc ++ "__" ++ eprotoc_generator:atom_to_name(Inside))
            end;
        _ ->
            false
    end;
enum_search(Type, Enums, Acc) ->
    case lists:keyfind(Type, 2, Enums) of
        {enum, _, _} ->
            {Acc, eprotoc_generator:atom_to_name(Type)};
        _ ->
            false
    end.

%% Search the messages of parsed proto file for the nested Message definition.
-spec message_search(term(), list(), list()) -> list() | false.
message_search({nested, Inside, Type}, Msgs, Acc) ->
    case lists:keyfind(Inside, 2, Msgs) of
        {message, _, {_, Msgs1}} ->
            message_search(Type, Msgs1, Acc ++ eprotoc_generator:atom_to_name(Inside) ++ "__");
        _ ->
            false
    end;
message_search(Type, Msgs, Acc) ->
    case lists:keyfind(Type, 2, Msgs) of
        {message, _, _} ->
            Acc ++ eprotoc_generator:atom_to_name(Type);
        _ ->
            false
    end.

nested_atom_to_name({nested, Inside, Type}, Acc) ->
    nested_atom_to_name(Type,Acc ++ eprotoc_generator:atom_to_name(Inside) ++ "__");
nested_atom_to_name(Type, Acc) ->
    Acc ++ eprotoc_generator:atom_to_name(Type).

get_field_type({nested, Inside, Type}, _, _, Proto) ->
    case lists:keysearch(Inside, 1, element(2, Proto)) of
        false ->
            PackageName = eprotoc_generator:atom_to_name(element(1, Proto)),
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
                            throw("Imported message " ++ eprotoc_generator:atom_to_name(Type) ++ " not found.");
                        _ ->
                            Level = eprotoc_generator:atom_to_name(Inside) ++ "__" ++ eprotoc_generator:atom_to_name(Type),
                            "{message, fun " ++ Level ++ ":decode/1, fun " ++ Level ++ ":encode/1}"
                    end
            end
    end;
get_field_type(TypeAtom, Enums, Messages, _) ->
    Type = eprotoc_generator:atom_to_name(TypeAtom),
    case eprotoc:wire_type(TypeAtom) of
        custom ->
            case {lists:keysearch(TypeAtom, 2, Enums),
                  lists:keysearch(TypeAtom, 2, Messages)} of
                {{value, {Level, EnumAtom}}, false} ->
                    %% Enums are treated like uint32 types on the wire
                    Enum = eprotoc_generator:atom_to_name(EnumAtom),
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