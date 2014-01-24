-module(jesse_ref_SUITE).

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([basic/1]).

-include_lib("common_test/include/ct.hrl").

-define(DATA,        <<"data">>).
-define(DESCRIPTION, <<"description">>).
-define(SCHEMAS,     <<"schemas">>).
-define(_SCHEMA,     <<"$schema">>).
-define(TESTS,       <<"tests">>).
-define(VALID,       <<"valid">>).

all() ->
  [
      basic
  ].

%%%
init_per_suite(Config) ->
  TestSpecs = load_test_specs(?config(data_dir, Config)),
  TestSpecs ++ Config.

end_per_suite(_Config) ->
  ok.

%%% Testcases
basic(Config) ->
  Key   = "basic",
  Specs = ?config(Key, Config),
  ok = run_tests(Specs).

%%% Internal functions
run_tests(Specs) ->
  lists:foreach( fun(Spec) ->
                     Description = get_path(?DESCRIPTION, Spec),
                     Schemas     = get_path(?SCHEMAS, Spec),
                     TestSet     = get_path(?TESTS, Spec),
                     io:format("** Test set: ~s~n", [Description]),
                     run_test_set(Schemas, TestSet)
                 end
               , Specs
               ).

run_test_set(Schemas, TestSet) ->
  lists:foreach(
    fun (Schema) ->
        Id =
            try
                jesse_schema_validator:get_schema_id(Schema)
            catch
                throw:_ -> ""
            end,
        case jesse:add_schema(list_to_binary(Id), Schema) of
            ok -> ok;
            {error, Err} -> throw(Err)
        end
    end,
    Schemas
  ),
  lists:foreach( fun(Test) ->
                     Description = get_path(?DESCRIPTION, Test),
                     TestData    = get_path(?DATA, Test),
                     io:format("* Test case: ~s~n", [Description]),
                     SchemaId =
                         case get_path(?_SCHEMA, TestData) of
                             [] -> <<>>;
                             Id -> Id
                         end,
                     Schema0 = jesse_database:read(SchemaId),
                     Result = jesse:validate_with_schema(Schema0, TestData),
                     io:format("Result: ~p~n", [Result]),
                     case get_path(?VALID, Test) of
                       true  -> {ok, TestData} = Result;
                       false -> {error, Error} = Result,
                                match_error(Error)
                     end
                 end
               , TestSet
               ).

load_test_specs(TestsDir) ->
  FileList = filelib:wildcard(TestsDir ++ "/*.json"),
  lists:map( fun(Filename) ->
                 {ok, Bin} = file:read_file(Filename),
                 JsonTest  = jiffy:decode(Bin),
                 {filename_to_key(Filename), JsonTest}
             end
           , FileList
           ).

filename_to_key(Filename) ->
  filename:rootname(filename:basename(Filename)).

get_path(Key, Schema) ->
  jesse_json_path:path(Key, Schema).

match_error({'data_invalid', _, Type, _}) -> match_error_type(Type);
match_error({'schema_invalid', _, Type}) -> match_error_type(Type);
match_error(Error) -> throw({'wrong_error_format', Error}).

match_error_type(Atom) when is_atom(Atom) -> ok;
match_error_type(Tuple) when is_tuple(Tuple)
                     andalso is_atom(element(1, Tuple)) -> ok;
match_error_type(Type) -> throw({'wrong_error_type', Type}).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
