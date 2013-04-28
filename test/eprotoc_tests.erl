-module(eprotoc_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok.
cleanup(_) ->
    ok.

eprotoc_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [
      fun test_decode_message/0,
      fun test_encode_varint_small/0,
      fun test_encode_varint_big/0,
      fun test_encode_sint32_negative/0,
      fun test_encode_sint32_positive/0,
      fun test_encode_sint64_negative/0,
      fun test_encode_sint64_positive/0,
      fun test_encode_string/0,
      fun test_encode_value_string/0,
      fun test_encode_value_varint/0,
      fun test_encode_message/0
     ]}.

test_decode_message() ->
    Result = eprotoc:decode_message(<< 8, 16#96, 1 >>),
    ?assertEqual([{1, 0, 150}], Result).

test_encode_varint_small() ->
    Result = eprotoc:encode_varint(5),
    ?assertEqual([5], Result).

test_encode_varint_big() ->
    Result = eprotoc:encode_varint(300),
    ?assertEqual([172, 2], Result).

test_encode_sint32_negative() ->
    Result = eprotoc:encode_sint32(-2),
    ?assertEqual([3], Result).

test_encode_sint32_positive() ->
    Result = eprotoc:encode_sint32(1),
    ?assertEqual([2], Result).

test_encode_sint64_negative() ->
    Result = eprotoc:encode_sint64(-2),
    ?assertEqual([3], Result).

test_encode_sint64_positive() ->
    Result = eprotoc:encode_sint64(1),
    ?assertEqual([2], Result).

test_encode_string() ->
    Result = eprotoc:encode_string(<<"testing">>),
    ?assertEqual([[7], <<"testing">>], Result).

test_encode_value_string() ->
    Result = eprotoc:encode_value(2, string, <<"testing">>),
    ?assertEqual([[16#12], [7], <<"testing">>], Result).

test_encode_value_varint() ->
    Result = eprotoc:encode_value(2, uint32, 300),
    ?assertEqual([[16], 172, 2], Result).

test_encode_message() ->
    Message = [{a, {1, uint32, 150}}],
    Result = eprotoc:encode_message(orddict:from_list(Message)),
    ?assertEqual([[[16#08], 16#96, 16#01]], Result).
