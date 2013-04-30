%% Proto file yecc grammar

Nonterminals
proto imports messages
p_enum enum_fields enum_field enum_name enum_value
p_message message_name
fields field field_rule field_num field_type field_name
options option.

Terminals
';' '=' '{' '}' '[' ']' ','
package default packed message enum atom string integer float var bool.

Rootsymbol proto.

proto -> package var ';' messages : {value_of('$2'), '$4'}.

messages -> p_enum : {['$1'], []}.
messages -> p_message : {[], ['$1']}.
messages -> p_enum messages : add_first('$1', '$2').
messages -> p_message messages : add_second('$1', '$2').

p_message -> message message_name '{' fields '}' : {message, '$2', '$4'}.

message_name -> var : value_of('$1').

fields -> p_enum : {['$1'], []}.
fields -> p_enum fields : add_first('$1', '$2').
fields -> field : {[], ['$1']}.
fields -> field fields : add_second('$1', '$2').

field -> p_message : '$1'.
field ->
    field_rule field_type field_name '=' field_num '[' options ']' ';'
        : {field, '$1', '$2', '$3', '$5', '$7'}.
field ->
    field_rule field_type field_name '=' field_num ';'
        : {field, '$1', '$2', '$3', '$5', []}.

field_rule -> atom : value_of('$1').

field_type -> atom : value_of('$1').
field_type -> var : value_of('$1').

field_name -> atom : value_of('$1').

field_num -> integer : value_of('$1').

options -> option : ['$1'].
options -> option ',' options : ['$1'|'$3'].

option -> packed '=' atom : {packed, value_of('$3')}.
option -> default '=' integer : {default, value_of('$3')}.
option -> default '=' float : {default, value_of('$3')}.
option -> default '=' string : {default, value_of('$3')}.
option -> default '=' var : {default, value_of('$3')}.
option -> default '=' bool : {default, value_of('$3')}.

p_enum -> enum enum_name '{' enum_fields '}' : {enum, '$2', '$4'}.

enum_fields -> enum_field : ['$1'].
enum_fields -> enum_field enum_fields : ['$1'|'$2'].

enum_field ->
    enum_name '=' enum_value ';' : {'$1', '$3'}.

enum_name -> atom : value_of('$1').
enum_name -> var : value_of('$1').

enum_value -> integer : integer_to_list(value_of('$1')).

Erlang code.
add_first(A, {As, Bs}) ->
    {[A|As], Bs}.

add_second(B, {As, Bs}) ->
    {As, [B|Bs]}.

value_of(Token) ->
    element(3, Token).
