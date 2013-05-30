%%%-----------------------------------------------------------------------------
%% @doc Unit tests for the error path
%%
%% @since 5/30/13
%% @author Andrey Latnikov <alatnikov@alertlogic.com>
%% @copyright 2013 Alert Logic, Inc.
%%%-----------------------------------------------------------------------------
-module(jesse_error_path_tests).
-author("Andrey Latnikov <alatnikov@alertlogic.com>").

-include_lib("eunit/include/eunit.hrl").
-record(book, {title, author, pages, isbn, chapters}).

schema() ->
    [
        {<<"type">>, <<"object">>},
        {<<"required">>, true},
        {<<"properties">>, [
            {<<"books">>, [
                {<<"type">>, <<"array">>},
                {<<"required">>, true},
                {<<"minItems">>, 1},
                {<<"items">>, [
                    {<<"type">>, <<"object">>},
                    {<<"patternProperties">>, [
                        {<<"^number[0-9]+$">>, [
                            {<<"type">>, <<"integer">>}
                        ]}
                    ]},
                    {<<"additionalProperties">>, [
                        {<<"type">>, <<"string">>}
                    ]},
                    {<<"properties">>, [
                        {<<"title">>, [
                            {<<"type">>, <<"string">>},
                            {<<"required">>, true}
                        ]},
                        {<<"author">>, [
                            {<<"type">>, <<"string">>},
                            {<<"required">>, true}
                        ]},
                        {<<"pages">>, [ {<<"type">>, <<"integer">>} ]},
                        {<<"isbn">>, [
                            {<<"type">>, [
                                <<"string">>,
                                <<"integer">>
                            ]}
                        ]},
                        {<<"chapters">>, [
                            {<<"type">>, <<"array">>},
                            {<<"minItems">>, 1},
                            {<<"uniqueItems">>, true},
                            {<<"items">>, [
                                {<<"type">>, <<"string">>}
                            ]}
                        ]}
                    ]}
                ]}
            ]}
        ]}
    ].


book() ->
    #book{title = <<"Thinking in Java">>,
          author = <<"Bruce Eckel">>,
          isbn = <<"ISBN 0-13-187248-6">>,
          pages = 1120,
          chapters = [
              <<"Chapter 1. Preface">>,
              <<"Chapter 2. Introduction">>,
              <<"Chapter 3. Introduction to Objects">>
          ]
    }.

is_defined({_Name, undefined}) -> [];
is_defined({Name, Value}) -> [{Name, Value}].

to_json(Books) when is_list(Books) ->
    Jsons = lists:map(fun to_json/1, Books),
    [ {<<"books">>, Jsons} ];
to_json(#book{title = Title, author = Author, isbn = Isbn,
              pages = Pages, chapters = Chapters}) ->
    lists:append([
        is_defined(V) || V <- [
            {<<"title">>, Title},
            {<<"author">>, Author},
            {<<"isbn">>, Isbn},
            {<<"pages">>, Pages},
            {<<"chapters">>, Chapters}
        ]
    ]).

accumulator(Path, _Error, List) ->
    [Path | List].

valid_test() ->
    Json = to_json([book()]),
    ?assertEqual({ok, Json}, jesse:validate_with_accumulator(schema(), Json)).

wrong_item_property_test() ->
    Json0 = to_json([
        book(), (book())#book{pages = <<"wrong pages">>}
    ]),
    ?assertEqual({error, ["books[1].pages"]},
                 jesse:validate_with_accumulator(schema(), Json0,
                                                 fun accumulator/3, [])).

wrong_item_test() ->
    Json = to_json([
        (book())#book{chapters = [
            <<"Chapter 1. Preface">>,
            <<"Chapter 2. Introduction">>,
            1234
        ]}
    ]),
    ?assertEqual({error, ["books[0].chapters[2]"]},
                 jesse:validate_with_accumulator(schema(), Json,
                                                 fun accumulator/3, [])).

missing_property_test() ->
    Json = to_json([
        (book())#book{title = undefined},
        book()
    ]),
    ?assertEqual({error, ["books[0]"]},
                 jesse:validate_with_accumulator(schema(), Json,
                                                 fun accumulator/3, [])).

additional_property_test() ->
    Json = [
        {<<"books">>, [
            to_json(book()),
            [{<<"wrongProperty">>, 1} | to_json((book()))]
        ]}
    ],
    ?assertEqual({error, ["books[1].wrongProperty"]},
                 jesse:validate_with_accumulator(schema(), Json,
                                                 fun accumulator/3, [])).

pattern_property_test() ->
    Json = [
        {<<"books">>, [
            to_json(book()),
            [{<<"number0">>, <<"string">>} | to_json((book()))]
        ]}
    ],
    ?assertEqual({error, ["books[1].number0"]},
                 jesse:validate_with_accumulator(schema(), Json,
                                                 fun accumulator/3, [])).

no_items_test() ->
    Json = [ {<<"books">>, []} ],
    ?assertEqual({error, [""]},
                 jesse:validate_with_accumulator(schema(), Json,
                                                 fun accumulator/3, [])).

union_type_test() ->
    Json = to_json([
        (book())#book{isbn = [1, 123, 10, 23]}
    ]),
    ?assertEqual({error, ["books[0].isbn"]},
                 jesse:validate_with_accumulator(schema(), Json,
                                                 fun accumulator/3, [])).

unique_items_test() ->
    Json = to_json([
        (book())#book{chapters = [
            <<"one">>,
            <<"two">>,
            <<"three">>,
            <<"three">>
        ]}
    ]),
    ?assertEqual({error, ["books[0].chapters"]},
                 jesse:validate_with_accumulator(schema(), Json,
                                                 fun accumulator/3, [])).
