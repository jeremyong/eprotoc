-module(eprotoc_generator).

-export([
         generate_parser/0,
         process_file/3,
         process_file/4,
         read_and_scan/1,
         generate_code/2,
         output_results/3,
         parse_file/4,
         reserved_words/1,
         atom_to_name/1
        ]).

generate_parser() ->
    yecc:file("src/proto_grammar.yrl").

%% @doc
%% Calls `process_file/4` where the fourth argument is the default Erlang
%% eprotoc code generator module (`eprotoc_codegen`).
%% @end
-spec process_file(list(), list(), list()) -> ok.
process_file(File, Outdir, ImportDirs) ->
    process_file(File, Outdir, ImportDirs, eprotoc_codegen).

%% @doc Processes a proto file in four steps. First, it parses the proto file
%% and generates the code. Next, it deletes existing modules that have been generated
%% before. Last, it outputs the results of the generated code to the supplied directory.
-spec process_file(list(), list(), list(), module()) -> ok.
process_file(File, Outdir, ImportDirs, Codegen) ->
    {module, _} = code:ensure_loaded(Codegen),
    Proto = parse_file(File, Outdir, ImportDirs, Codegen),
    Result = generate_code(Proto, Codegen),
    Ext = Codegen:file_ext(),
    delete_existing_files(Outdir, Result, Ext),
    output_results(Outdir, Result, Ext).

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
-spec handle_imports(tuple(), list(), list(), module()) -> tuple().
handle_imports({Name, Imports, L}, Outdir, ImportDirs, Codegen) ->
    Imported = 
        lists:foldl(fun(Import, Acc) ->
                            case find_file(Import, ImportDirs) of
                                error ->
                                    throw(Import ++ " not found");
                                File ->
                                    {ImportName, Imports1, L1} = parse_file(File, Outdir, ImportDirs, Codegen),
                                    case [ImportName == I || {I, _, _} <- Acc] of
                                        [] ->
                                            Result = generate_code({ImportName, Imports1, L1}, Codegen),
                                            Ext = Codegen:file_ext(),
                                            delete_existing_files(Outdir, Result, Ext),
                                            output_results(Outdir, Result, Ext),
                                            Acc ++ [{ImportName, [], L1}| Imports1];
                                        _ ->
                                            Acc ++ Imports1
                                    end
                            end
                    end, [], Imports),
    {Name, Imported, L}.

-spec generate_code(tuple(), module()) -> list().
generate_code(Proto, Codegen) ->
    PackageName = atom_to_name(element(1, Proto)),
    Package = element(3, Proto),
    {_, Result} = peel(Package, {PackageName, [], []}, [], Proto, Codegen),
    Result.

-spec parse_file(list(), list(), list(), module()) -> tuple().
parse_file(File, Outdir, ImportDirs, Codegen) ->
    Tokens = read_and_scan(File),
    {ok, Proto} = proto_grammar:parse(Tokens),
    handle_imports(Proto, Outdir, ImportDirs, Codegen).

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

delete_existing_files(Dir, [{Module, _}|Rest], Ext) ->
    Filepath = get_filepath(Dir, Module, Ext),
    case filelib:is_file(Filepath) of
        false ->
            ok;
        true ->
            file:delete(Filepath)
    end,
    delete_existing_files(Dir, Rest, Ext);
delete_existing_files(_, [], _) ->
    ok.

output_results(Dir, [], _) ->
    io:format("Output finished to directory ~p~n", [Dir]),
    ok;
output_results(Dir, [{Module,Text}|Rest], Ext) ->
    Filepath = get_filepath(Dir, Module, Ext),
    case filelib:is_file(Filepath) of
        false ->
            file:write_file(Filepath,
                            "-module(" ++ Module ++ ").\n"
                            "-compile(export_all).\n\n");
        true ->
            ok
    end,
    file:write_file(Filepath, Text, [append]),
    output_results(Dir, Rest, Ext).

%% hidden
get_filepath(Dir, File, Ext) ->
    Dir ++ "/" ++ File ++ Ext.


%% @doc Hidden. MFs refers to messages or fields (they may be at the same nesting level.
%% The second argument is a nesting accumulator of the form:
%% {nesting level, enums available at this scope, messages available at this scope}
%% The nesting level doubles as the module name of the current scope
%% This code essentially functions like an onion (albeit a complicated one),
%% hence the name.
peel({[], []}, NestingAcc, Acc, _, _) -> {NestingAcc, Acc};
peel([], NestingAcc, Acc, _, _) -> {NestingAcc, Acc};
%% Peel all enums first
peel({[{enum, Enum, Fields}|Enums], MFs},
     {Level, AccEnums, AccMessages}, Acc, Proto, Codegen) ->
    EnumName = atom_to_name(Enum),
    Text = Codegen:generate_enum(EnumName, Fields, ""),
    Acc1 = [{Level, Text}|Acc],
    %% Make this enum available to everything at this nesting level or deeper.
    AccEnums1 = [{Level, Enum}|AccEnums],
    peel({Enums, MFs}, {Level, AccEnums1, AccMessages}, Acc1, Proto, Codegen);
%% Because we parse a message immediately, we need to look ahead to find messages
%% at the same nesting level. We do this right after parsing the enums.
peel({[], MFs}, {Level, AccEnums, AccMessages}, Acc, Proto, Codegen) ->
    Messages = lists:filter(fun(MF) ->
                                    element(1, MF) == message
                            end, MFs),
    Messages1 = lists:map(fun(Msg) ->
                                  MessageAtom = element(2, Msg),
                                  MessageName = atom_to_name(MessageAtom),
                                  {Level ++ "__" ++ MessageName, MessageAtom}
                          end, Messages),
    AccMessages1 = Messages1 ++ AccMessages,
    peel(MFs, {Level, AccEnums, AccMessages1}, Acc, Proto, Codegen);
peel([{message, Message, {Enums, Fields}}|MFs],
     {Level, AccEnums, AccMessages}, Acc, Proto, Codegen) ->
    MessageName = atom_to_name(Message),
    Level1 = Level ++ "__" ++ MessageName,
    %% Generate message encoding and decoding functions with everything available
    %% at this level of nesting thus far.
    {{_, AccEnums1, AccMessages1}, Acc1} =
        peel({Enums, Fields}, {Level1, AccEnums, AccMessages}, Acc, Proto, Codegen),
    Text = Codegen:generate_message(Fields, AccEnums1, AccMessages1, Proto),
    %% Make this message available to everything at this nesting level or deeper.
    %% Note that the messages added to the scope when parsing the message contents
    %% are thrown away!
    AccMessages2 = [{Level1, Message}|AccMessages],
    peel(MFs, {Level, AccEnums, AccMessages2}, [{Level1, Text}|Acc1], Proto, Codegen);
peel([{field, Rule, Type, AtomName, Num, Opts}|MFs],
     {Level, AccEnums, AccMessages}, Acc, Proto, Codegen) ->
    Name = atom_to_list(AtomName),
    Text = Codegen:generate_field(Rule, Name, Num, Opts, Type),
    Acc1 = [{Level, Text}|Acc],
    peel(MFs, {Level, AccEnums, AccMessages}, Acc1, Proto, Codegen).

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
