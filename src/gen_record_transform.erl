%%--------------------------------------------------------------------------------------------------
-module(gen_record_transform).
%%--------------------------------------------------------------------------------------------------
-export([
  parse_transform/2
]).
%%--------------------------------------------------------------------------------------------------
-import(trans_utils, [
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
%% TODO: return old AST if no record name = module name
%%--------------------------------------------------------------------------------------------------
-type ast() :: list(term()).
-type proplist() :: list(tuple()).
%%--------------------------------------------------------------------------------------------------

-spec parse_transform(ast(), proplist()) -> ast().

parse_transform(AST, _Options) ->
  % io:format("Opts: ~p \n\n", [Options]),

  % io:format("IN AST: ~p \n\n", [AST]),  
  
  NewAST = case get_attribute(gen_record, AST) of
    {ok, RecordName} when is_atom(RecordName) ->
      case get_record(RecordName, AST) of
        {ok, Record} ->
          inject_code(RecordName, Record, AST);
        _ ->
          AST
        end;
    _ -> 
      AST
  end,

  % io:format("OUT AST: ~p \n\n", [NewAST]),
  NewAST.

%%--------------------------------------------------------------------------------------------------

inject_code(_RecordName, Record, AST) ->
  ModuleName      = get_module_name(AST),
  _LastLineNumber = get_last_line_number(AST),
  RecordFields    = get_record_fields(Record),

  % {AST1, _} = insert_field_ast_fun(AST, RecordFields),
  % {AST2, _} = insert_fields_ast_fun(AST1, RecordFields),
  % {AST3, _} = insert_num_of_fields_ast_fun(AST2, RecordFields),
  AST1 = insert_get_funs(ModuleName, RecordFields, AST),
  AST2 = insert_set_funs(ModuleName, RecordFields, AST1),
  AST3 = insert_init_fun(AST2),
  AST4 = insert_new_fun(ModuleName, AST3),

  AST4.


%%--------------------------------------------------------------------------------------------------
%% TODO when is record

insert_get_funs(ModuleName, Fields, AST) when is_list(Fields) ->
  X = fun(Field, AccIn) ->
    AccOut = insert_get_fun(ModuleName, Field, AccIn),
    AccOut
  end,
  lists:foldl(X, AST, Fields).

insert_get_fun(ModuleName, Field, AST) ->
  LastLineNumber = get_last_line_number(AST),
  case has_function(Field, 2, AST) of
    false ->
      Str = lists:flatten(
        io_lib:format("~s(Rec) -> Rec#~s.~s .\n",[Field, ModuleName, Field])
      ),
      {ElementAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
      insert_before_eof(AST, ElementAST, NewLastLineNumber);
    _ ->
      AST
  end.

%%--------------------------------------------------------------------------------------------------

insert_set_funs(ModuleName, Fields, AST) when is_list(Fields) ->
  X = fun(Field, AccIn) ->
    AccOut = insert_set_fun(ModuleName, Field, AccIn),
    AccOut
  end,
  lists:foldl(X, AST, Fields).

insert_set_fun(ModuleName, Field, AST) ->
  LastLineNumber = get_last_line_number(AST),
  case has_function(Field, 2, AST) of
    false ->
      Str = lists:flatten(
        io_lib:format("~s(Rec, Val) -> Rec#~s{ ~s = Val } .\n",[Field, ModuleName, Field])
      ),
      {ElementAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
      insert_before_eof(AST, ElementAST, NewLastLineNumber);
    _ ->
      AST
  end.
  
%%--------------------------------------------------------------------------------------------------

%% @doc Check if init/1 function exists, if not it will insert one
-spec insert_init_fun( ast() ) -> ast().

insert_init_fun(AST) ->
  LastLineNumber = get_last_line_number(AST),

  case has_function(init, 1, AST) of
    false -> 
      Str = "init(Record) -> Record .\n",
      {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
      insert_before_eof(AST, InsertAST, NewLastLineNumber);

    true ->
      AST
  end.

%%--------------------------------------------------------------------------------------------------

%% @doc Inserts new/0 function that will call init(Record) and return record.
-spec insert_new_fun( atom(), ast() ) -> ast().

%% TODO validate input and output?

insert_new_fun(ModuleName, AST) ->
  LastLineNumber = get_last_line_number(AST),
  case has_function(new, 0, AST) of
    false ->
      Str = lists:flatten(
        io_lib:format("new() -> Record = #~s{} , init(Record) .\n", [ModuleName])
      ),
      {ElementAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
      insert_before_eof(AST, ElementAST, NewLastLineNumber);
    
    _ ->
      AST
  end.
  
%%--------------------------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------------------------
%% @doc This function will convert AST

% -spec parse_transform1(ast(), proplist()) -> ast().
% parse_transform1(AST, _Options) ->
%   % io:format("AST: \n~p\n",[AST]),
%   NewAST = try
%     _LastLineNumber = get_last_line_number(AST),
%     ModuleName     = get_module_name(AST),
%     ModuleRecord   = get_record(ModuleName, AST),
%     RecordFields   = get_record_fields(ModuleRecord),
%     % io:format("LastLineNumber: ~p\n",[LastLineNumber]),
%     % io:format("ModuleName: ~p\n",[ModuleName]),
%     % io:format("ModuleRecord: ~p\n",[ModuleRecord]),
%     % io:format("RecordFields: ~p\n",[RecordFields]),

%     {AST1, _} = insert_field_ast_fun(AST, RecordFields),
%     {AST2, _} = insert_fields_ast_fun(AST1, RecordFields),
%     {AST3, _} = insert_num_of_fields_ast_fun(AST2, RecordFields),  
%     {AST4, _} = insert_record_ast_fun(AST3, ModuleName),
%     {AST5, _} = insert_init_ast_fun(AST4),
%     {AST6, _} = insert_pk_ast_fun(AST5),
%     {AST7, _} = insert_serial_ast_fun(AST6),

%     %%io:format("NewAST: ~p\n",[AST7]),
%     AST7

%   catch
%     throw:{invalid_record_field, Params} ->
%       io:format("Unable to parse record field: ~p\n",[proplists:get_value(field,Params)]),
%       throw(stop);

%     throw:{record_does_not_exist, Params} ->
%       io:format("Record: ~p does not exist!\n",[proplists:get_value(record,Params)]),
%       AST;

%     C:E ->
%       io:format("Class: ~p, Excaption: ~p\nStacktrace: ~p\n",[C,E,erlang:get_stacktrace()]),
%       throw(stop)
%   end,
%   NewAST.

% %%--------------------------------------------------------------------------------------------------

% %% @doc Inserts '=field(Field)' function that returns field index

% -spec insert_field_ast_fun(ast(), list(atom())) -> tuple(ast(), integer()).
% insert_field_ast_fun(AST, Fields) ->
%   LastLineNumber = get_last_line_number(AST),
%   {InsertAST, NewLastLineNumber} = insert_field_ast_fun_loop(Fields, LastLineNumber, 2, []),
%   insert_before_eof(AST, InsertAST, NewLastLineNumber).

% insert_field_ast_fun_loop([], LastLineNumber, _, Acc) -> 
%   Acc1 = Acc ++ "'=field'(Field) -> throw({invalid_field,[{field, Field}]}). \n",
%   from_str_to_ast(Acc1, LastLineNumber);
  
% insert_field_ast_fun_loop([Field|Fields], LastLineNumber, Index, Acc) ->
%   Acc1 = Acc ++ "'=field'(" ++ atom_to_list(Field) ++") -> "++ integer_to_list(Index) ++"; \n",
%   insert_field_ast_fun_loop(Fields, LastLineNumber, Index+1, Acc1).

% %%--------------------------------------------------------------------------------------------------

% %% @doc Inserts '=fields'() function that returns a list of fields

% -spec insert_fields_ast_fun(ast(), list(atom())) -> tuple(ast(), integer()).
% insert_fields_ast_fun(AST, Fields) ->
%   LastLineNumber = get_last_line_number(AST),
%   Lst = "'=fields'() -> [ " ++ (lists:flatten([ ", " ++ atom_to_list(F) || F <- Fields]) -- "," ) ++ " ].",
%   {InsertAST, NewLastLineNumber} = from_str_to_ast(Lst,LastLineNumber),
%   insert_before_eof(AST, InsertAST, NewLastLineNumber).

% %%--------------------------------------------------------------------------------------------------

% %% @doc Inserts '=record'() function that returns a record

% -spec insert_record_ast_fun(ast(), atom()) -> tuple(ast(), integer()).
% insert_record_ast_fun(AST, RecordName) ->
%   LastLineNumber = get_last_line_number(AST),
%   Str = "'=record'() -> #"++ atom_to_list(RecordName) ++"{} .",
%   {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
%   insert_before_eof(AST, InsertAST, NewLastLineNumber).



% %% @doc Inserts a function '=fields_num'() that will return number of fields defined in a record

% -spec insert_num_of_fields_ast_fun(ast(), list(atom())) -> tuple(ast(), integer()).
% insert_num_of_fields_ast_fun(AST, RecordFields) ->
%   LastLineNumber = get_last_line_number(AST),

%   Len = length(RecordFields),
%   Str = "'=num_of_fields'() -> "++ integer_to_list(Len) ++".",
%   {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
%   insert_before_eof(AST, InsertAST, NewLastLineNumber).

% %%--------------------------------------------------------------------------------------------------

% %% @doc Check if '=pk'() functions exists, if not it will insert one

% -spec insert_pk_ast_fun(ast()) -> tuple(ast(), integer()).
% insert_pk_ast_fun(AST) ->
%   LastLineNumber = get_last_line_number(AST),

%   case has_function('=pk', 0, AST) of
%     false -> 
%       Str = "'=pk'() -> undefined.",
%       {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
%       insert_before_eof(AST, InsertAST, NewLastLineNumber);
%     true ->
%       {AST, LastLineNumber}
%   end.

% %%--------------------------------------------------------------------------------------------------

% %% @doc Check if '=serial'() functions exists, if not it will insert one

% -spec insert_serial_ast_fun(ast()) -> tuple(ast(), integer()).
% insert_serial_ast_fun(AST) ->
%   LastLineNumber = get_last_line_number(AST),

%   case has_function('=serial', 0, AST) of
%     false -> 
%       Str = "'=serial'() -> undefined.",
%       {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
%       insert_before_eof(AST, InsertAST, NewLastLineNumber);
%     true ->
%       {AST, LastLineNumber}
%   end.

% %%--------------------------------------------------------------------------------------------------