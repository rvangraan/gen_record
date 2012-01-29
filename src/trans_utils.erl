%%--------------------------------------------------------------------------------------------------
-module(trans_utils).
%%--------------------------------------------------------------------------------------------------
-export([
  get_last_line_number/1,
  get_module_name/1,
  get_attribute/2,
  has_function/3,
  get_record/2,
  get_record_fields/1,
  from_str_to_ast/2,
  insert_before_eof/3
]).
%%--------------------------------------------------------------------------------------------------
-type ast()      :: list(term()).
%%--------------------------------------------------------------------------------------------------

%% @doc Returns last line number from AST
-spec get_last_line_number( ast() ) -> integer().

get_last_line_number([ {eof, LineNum} | _ ]) -> 
  LineNum;
get_last_line_number([ _ | AST ]) -> 
  get_last_line_number(AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns module name from AST
-spec get_module_name( ast() ) -> atom().

get_module_name([ {attribute, _, module, ModuleName} | _ ]) -> 
  ModuleName;
get_module_name([ _ | AST ]) -> 
  get_module_name(AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns attribute options if attribute is found
-spec get_attribute(atom(), ast()) -> error | {ok, ast()}.

get_attribute(_Attribute, []) ->
  error;
get_attribute(Attribute, [ {attribute, _, Attribute, AttributeOpts} | _ ]) ->
  {ok, AttributeOpts};
get_attribute(Attribute, [ _ | AST ]) ->
  get_attribute(Attribute, AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Checks if AST has function/arity
-spec has_function( atom(), integer(), ast() ) -> boolean().

has_function(_FunctionName, _Arity, []) -> 
  false;
has_function(FunctionName, Arity, [ {function, _, FunctionName, Arity, _} | _ ]) -> 
  true;
has_function(FunctionName, Arity, [ _ | AST ]) -> 
  has_function(FunctionName, Arity, AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns record AST if its found
-spec get_record( atom(), ast() ) -> error | {ok, ast()}.

get_record(_RecordName, []) -> 
  error;
get_record(RecordName, [ {attribute, _, record, {RecordName, _}=Record} | _ ]) -> 
  {ok, Record};
get_record(RecordName, [ _ | AST ]) -> 
  get_record(RecordName, AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns record fields
-spec get_record_fields( ast() ) -> list(atom()).

get_record_fields({ _RecordName, RecordFields }=_ASTElement) ->
  get_record_fields_loop(RecordFields, []).

get_record_fields_loop([],Acc) -> 
  lists:reverse(Acc);
get_record_fields_loop([ {record_field,_,{atom, _, FieldName}} | AST], Acc) -> 
  get_record_fields_loop(AST, [FieldName | Acc]);
get_record_fields_loop([ {record_field, _, {atom, _, FieldName}, _DefaultValue} | AST ], Acc) -> 
  get_record_fields_loop(AST,[ FieldName | Acc]);
get_record_fields_loop([ Field | _AST ], _Acc) -> 
  throw({invalid_record_field, [ {field, Field} ]}).

%%--------------------------------------------------------------------------------------------------

%% @doc Takes erlang code as a string and LastLineNumber, and returns an AST with new last line number
-spec from_str_to_ast( string(), integer() ) -> tuple( ast(), integer() ).

from_str_to_ast(Str, LastLineNumber) ->
  {ok, Tokens, NewLastLineNumber} = erl_scan:string(Str, LastLineNumber),
  {ok, AST} = erl_parse:parse_form(Tokens),                             
  {AST, NewLastLineNumber}.

%%--------------------------------------------------------------------------------------------------

%% @doc Inserts AST before end of file
-spec insert_before_eof( ast(), ast(), integer() ) -> ast().

insert_before_eof(AST, InsertAST, NewLastLineNumber) ->
  insert_before_eof_loop(AST, InsertAST, NewLastLineNumber,[]).

insert_before_eof_loop([ {eof, _LastLineNumber} | _ ], InsertAST, NewLastLineNumber, Acc) -> 
  Acc1 = [ {eof, NewLastLineNumber} , InsertAST | Acc],
  lists:reverse(Acc1);

insert_before_eof_loop([ASTElement | AST], InsertAST, NumberOfLines, Acc) -> 
  insert_before_eof_loop(AST, InsertAST, NumberOfLines, [ASTElement | Acc]).

%%--------------------------------------------------------------------------------------------------