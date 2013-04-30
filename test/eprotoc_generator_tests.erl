-module(eprotoc_generator_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok.
cleanup(_) ->
    ok.

eprotoc_generator_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [
      fun test_encode_message/0,
      fun test_decode_message/0,
      fun test_encode_nested_message/0,
      fun test_decode_nested_message/0,
      fun test_encode_enum_message/0,
      fun test_decode_enum_message/0
     ]}.

test_encode_message() ->
    %% Message Test1 with value a = 150
    Message = [{a, {1, uint32, 150}}],
    Result = list_to_binary(test__test1:encode(Message)),
    ?assertEqual(<<8,150,1>>, Result).

test_decode_message() ->
    Payload = <<8,150,1>>,
    Result = test__test1:decode(Payload),
    ?assertEqual([{a, {1, uint32, 150}}], Result).

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
