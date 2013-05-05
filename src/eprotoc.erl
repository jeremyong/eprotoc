-module(eprotoc).

-export([
         decode_message/1,
         pop_varint/1,
         pop_string/1,
         pop_32bits/1,
         pop_64bits/1,
         encode_message/1,
         encode_value/3,
         encode_string/1,
         encode_sint32/1,
         encode_sint64/1,
         encode_varint/1,
         reverse_repeated_fields/1,
         wire_type/1,
         wire_encode_fun/1,
         wire_decode_fun/1,
         cast_type/2
        ]).

-type wire_type() :: 0 | 1 | 2 | 5.

%% @doc Decodes a supplied binary message into a list of values.
%% Each value is of the form {field num, wire type, Value}.
-spec decode_message(binary()) -> list().
decode_message(Message) ->
    lists:reverse(decode_message(Message, [])).

%% hidden
decode_message(<<>>, Acc) -> Acc;
decode_message(Message, Acc) ->
    {Varint, Rest} = pop_varint(Message),
    FNum = Varint bsr 3,
    WireType = Varint - (FNum bsl 3),
    Fun = wire_decode_fun(WireType),
    {Value, Rest1} = Fun(Rest),
    decode_message(Rest1, [{FNum, WireType, Value}|Acc]).

%% @doc Pops a varint off the front of the payload (assumed big endian)
-spec pop_varint(binary()) -> {integer(), binary()}.
pop_varint(Payload) ->
    pop_varint(Payload, 0, 0).

%% hidden
pop_varint(<< 1:1, Data:7, Rest/binary >>, Acc, Iter) ->
    pop_varint(Rest, (Data bsl (Iter * 7)) + Acc, Iter + 1);
pop_varint(<< 0:1, Data:7, Rest/binary >>, Acc, Iter) ->
    {(Data bsl (Iter * 7)) + Acc, Rest}.

pop_string(Payload) ->
    {Len, Rest} = pop_varint(Payload),
    << String:Len/binary-unit:8, Rest1/binary >> = Rest,
    {String, Rest1}.

pop_32bits(<< Value:32/binary, Rest/binary >>) ->
    {Value, Rest}.

pop_64bits(<< Value:64/binary, Rest/binary >>) ->
    {Value, Rest}.

%% @doc Raw decoded messages are keylists where the keys are
%% the field names, and the values are tuples of {field number,
%% type, and value}. In the event that the value is a repeated
%% field, the value is instead a list of raw decoded messages.
%% O(n)
-spec encode_message(list()) -> list().
encode_message(Message) ->
    lists:reverse(encode_message(Message, [])).

%% hidden
-spec encode_message(list(), list()) -> list().
encode_message([], Acc) -> Acc;
encode_message([{_, {FNum, Type, {repeated, Values}}}|Rest], Acc) ->
    Res = lists:map(fun(Value) ->
                            encode_value(FNum, Type, Value)
                    end, Values),
    encode_message(Rest, [Res|Acc]);
encode_message([{_, {FNum, Type, Value}}|Rest], Acc) ->
    Res = encode_value(FNum, Type, Value),
    encode_message(Rest, [Res|Acc]).

%% @doc Packs data along with the field num and wire type.
-spec encode_value(integer(), atom() | tuple(),
                   binary() | integer() | float()) ->
    iolist().
encode_value(_, _, undefined) ->
    [];
encode_value(FieldNum, {enum, EncodeFun}, Data) ->
    Num = EncodeFun(Data),
    [encode_varint((FieldNum bsl 3) bor 0),encode_varint(Num)];
encode_value(FieldNum, {message, EncodeFun}, Data) ->
    %% For message types, an encoding function is provided and the
    %% wire type is 2 for a fixed size packet
    Message = EncodeFun(Data),
    Size = iolist_size(Message),
    [encode_varint((FieldNum bsl 3) bor 2),encode_varint(Size),Message];
encode_value(FieldNum, Type, Data) ->
    WireType = wire_type(Type),
    Fun = wire_encode_fun(Type),
    [encode_varint((FieldNum bsl 3) bor WireType), Fun(Data)].

%% @doc Strings are preceded by a varint encoded length.
-spec encode_string(binary()) -> iolist().
encode_string(String) ->
    Len = byte_size(String),
    [encode_varint(Len), String].

%% @doc Signed integers are spiralled and varint encoded.
-spec encode_sint32(integer()) -> iolist().
encode_sint32(SInt) when SInt =< 16#ff, SInt >= -16#ff ->
    Int = (SInt bsl 1) bxor (SInt bsr 31),
    encode_varint(Int).

%% @doc Signed integers are spiralled and varint encoded.
-spec encode_sint64(integer()) -> iolist().
encode_sint64(SInt) when SInt =< 16#ffff, SInt >= -16#ffff ->
    Int = (SInt bsl 1) bxor (SInt bsr 63),
    encode_varint(Int).

%% @doc Encodes a supplied integer as a binary varint.
-spec encode_varint(integer()) -> iolist().
encode_varint(Int) when Int >= 0 ->
    lists:reverse(encode_varint(Int, [])).

%% hidden
encode_varint(true, Acc) ->
    [1|Acc];
encode_varint(false, Acc) ->
    [0|Acc];
encode_varint(Int, Acc) when Int =< 127->
    %% Fewer than seven bits left. End of the line.
    [Int|Acc];
encode_varint(Int, Acc) ->
    NextInt = Int bsr 7,
    LastSevenBits = Int - (NextInt bsl 7),
    %% More bytes to follow. Set the continuation bit
    Acc1 = [(1 bsl 7) + LastSevenBits|Acc],
    encode_varint(NextInt, Acc1).

%% Repeated fields are reversed upon decoding. This remedies that.
-spec reverse_repeated_fields(list()) -> list().
reverse_repeated_fields(Fields) ->
    lists:map(fun({F, Fs}) ->
                      case Fs of
                          {N, T, {repeated, Vs}} ->
                              {F, {N, T, {repeated, lists:reverse(Vs)}}};
                          _ -> {F, Fs}
                      end
              end, Fields).

-spec wire_type(atom()) -> custom | wire_type().
wire_type(int32) -> 0;
wire_type(int64) -> 0;
wire_type(uint32) -> 0;
wire_type(uint64) -> 0;
wire_type(sint32) -> 0;
wire_type(sint64) -> 0;
wire_type(bool) -> 0;
wire_type(enum) -> 0;
wire_type(fixed64) -> 1;
wire_type(sfixed64) -> 1;
wire_type(double) -> 1;
wire_type(string) -> 2;
wire_type(bytes) -> 2;
wire_type(embedded) -> 2;
wire_type(repeated) -> 2;
wire_type(fixed32) -> 5;
wire_type(sfixed32) -> 5;
wire_type(float) -> 5;
wire_type(_) -> custom.

-spec wire_encode_fun(atom()) -> function().
wire_encode_fun(uint32) -> fun encode_varint/1;
wire_encode_fun(uint64) -> fun encode_varint/1;
wire_encode_fun(sint32) -> fun encode_sint32/1;
wire_encode_fun(sint64) -> fun encode_sint64/1;
wire_encode_fun(string) -> fun encode_string/1;
wire_encode_fun(bytes) -> fun encode_string/1;
wire_encode_fun(bool) -> fun encode_varint/1.

-spec wire_decode_fun(wire_type()) -> function().
wire_decode_fun(0) -> fun pop_varint/1;
wire_decode_fun(1) -> fun pop_64bits/1;
wire_decode_fun(2) -> fun pop_string/1;
wire_decode_fun(5) -> fun pop_32bits/1.

cast_type(int32, Value) ->
    case Value band 16#8000000000000000 =/= 0 of
        true ->
            Value - 16#8000000000000000;
        _ ->
            Value
    end;
cast_type(int64, Value) ->
    case Value band 16#8000000000000000 =/= 0 of
        true ->
            Value - 16#8000000000000000;
        _ ->
            Value
    end;
cast_type(uint32, Value) ->
    Value;
cast_type(uint64, Value) ->
    Value;
cast_type(sint32, Value) ->
    (Value bsr 1) bxor (-(Value band 1));
cast_type(sint64, Value) ->
    (Value bsr 1) bxor (-(Value band 1));
cast_type(string, Value) ->
    Value;
cast_type(bool, 1) ->
    true;
cast_type(bool, 0) ->
    false;
cast_type(fixed64, << Value:64/little-unsigned-integer >>) ->
    Value;
cast_type(sfixed64, << Value:64/little-unsigned-integer >>) ->
    (Value bsr 1) bxor (-(Value band 1));
cast_type(double, << Value/little-float >>) ->
    Value;
cast_type(fixed32, << Value:32/little-unsigned-integer >>) ->
    binary:decode_unsigned(Value);
cast_type(sfixed32, << Value:32/little-unsigned-integer >>) ->
    (Value bsr 1) bxor (-(Value band 1));
cast_type(float, << Value/little-float >>) ->
    Value.
