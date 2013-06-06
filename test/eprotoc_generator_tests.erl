-module(eprotoc_generator_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok.
cleanup(_) ->
    ok.

eprotoc_generator_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [
      fun test_code_generation/0,
      fun test_encode_message/0,
      fun test_decode_message/0,
      fun test_set_message/0,
      fun test_get_message/0,
      fun test_get_unset_message/0,
      fun test_get_unset_repeated_message/0,
      fun test_encode_nested_message/0,
      fun test_decode_nested_message/0,
      fun test_encode_enum_message/0,
      fun test_decode_enum_message/0,
      fun test_encode_repeated_message/0,
      fun test_decode_repeated_message/0,
      fun test_encode_nested_repeated_message/0,
      fun test_decode_nested_repeated_message/0,
      fun test_encode_bool_message/0,
      fun test_decode_bool_message/0,
      fun test_encode_undefined_message/0,
      fun test_decode_undefined_message/0
     ]}.

test_code_generation() ->
    %% Not an actual test per se but gives an idea of code coverage
    %% on the generator code and ensures that the generation doesn't error
    eprotoc_generator:process_file("../test/test.proto", "/dev/null").

test_encode_message() ->
    %% Message Test1 with value a = 150
    Message = [{a, {1, uint32, 150}}],
    Result = list_to_binary(test__test1:encode(Message)),
    ?assertEqual(<<8,150,1>>, Result).

test_decode_message() ->
    Payload = <<8,150,1>>,
    Result = test__test1:decode(Payload),
    ?assertEqual([{a, {1, uint32, 150}}], Result).

test_set_message() ->
    Message = test__test1:s_a([], 5),
    ?assertEqual([{a, {1, uint32, 5}}], Message).

test_get_message() ->
    Payload = <<8,150,1>>,
    Result = test__test1:decode(Payload),
    Value = test__test1:g_a(Result),
    ?assertEqual(150, Value).

test_get_unset_message() ->
    Result = test__test7:g_g([]),
    ?assertEqual(true, Result).

test_get_unset_repeated_message() ->
    Result = test__test5:g_e([]),
    ?assertEqual([], Result).

test_encode_nested_message() ->
    %% Nested Test1 with value a = 150 inside Test3 message.
    Message = [{c, {3, {message, fun test__test1:encode/1},
                    [{a, {1, uint32, 150}}]}}],
    Result = list_to_binary(test__test3:encode(Message)),
    ?assertEqual(<<26,3,8,150,1>>, Result).

test_decode_nested_message() ->
    Payload = <<26,3,8,150,1>>,
    Result = test__test3:decode(Payload),
    Message = [{c, {3, {message, fun test__test1:encode/1},
                    [{a, {1, uint32, 150}}]}}],
    ?assertEqual(Message, Result).

test_encode_enum_message() ->
    Message = [{b, {1, {enum, fun test__test2:foo_enum/1}, bar}}],
    Result = list_to_binary(test__test2:encode(Message)),
    ?assertEqual(<<8,150,1>>, Result).

test_decode_enum_message() ->
    Payload = <<8,150,1>>,
    Result = test__test2:decode(Payload),
    Message = [{b, {1, {enum, fun test__test2:foo_enum/1}, bar}}],
    ?assertEqual(Message, Result).

test_encode_repeated_message() ->
    Message = [{d,{1,uint32,{repeated,[250,150]}}}],
    Result = list_to_binary(test__test4:encode(Message)),
    ?assertEqual(<<8,250,1,8,150,1>>, Result).

test_decode_repeated_message() ->
    Payload = <<8,250,1,8,150,1>>,
    Result = test__test4:decode(Payload),
    Message = [{d,{1,uint32,{repeated,[250,150]}}}],
    ?assertEqual(Message, Result).

test_encode_nested_repeated_message() ->
    Message = [{e,{1,
                   {message,fun test__test5__test6:encode/1},
                   {repeated,[[{f,{1,uint32,150}}],[{f,{1,uint32,300}}]]}}}],
    Result = list_to_binary(test__test5:encode(Message)),
    ?assertEqual(<<10,3,8,150,1,10,3,8,172,2>>, Result).

test_decode_nested_repeated_message() ->
    Payload = <<10,3,8,150,1,10,3,8,172,2>>,
    Result = test__test5:decode(Payload),
    Message = [{e,{1,
                   {message,fun test__test5__test6:encode/1},
                   {repeated,[[{f,{1,uint32,150}}],[{f,{1,uint32,300}}]]}}}],
    ?assertEqual(Message, Result).

test_encode_bool_message() ->
    %% Message Test1 with value a = 150
    Message = [{g, {1, bool, true}}],
    Result = list_to_binary(test__test7:encode(Message)),
    ?assertEqual(<<8,1>>, Result).

test_decode_bool_message() ->
    Payload = <<8,1>>,
    Result = test__test7:decode(Payload),
    ?assertEqual([{g, {1, bool, true}}], Result).

test_encode_undefined_message() ->
    %% Message Test1 with value a = 150
    Message = [{g, {1, bool, undefined}}],
    Result = list_to_binary(test__test7:encode(Message)),
    ?assertEqual(<<>>, Result).

test_decode_undefined_message() ->
    Payload = <<>>,
    Result = test__test7:decode(Payload),
    ?assertEqual([], Result).
