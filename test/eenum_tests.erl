%%------------------------------------------------------------------------------
%% Copyright 2012 Krzysztof Rutka
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%------------------------------------------------------------------------------

%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
%% @copyright 2012 Krzysztof Rutka
%% @doc Eunit test suite for eenum.
%% @private
-module(eenum_tests).

-compile({parse_transform, eenum}).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Enumerations
%%------------------------------------------------------------------------------

-enum({simple_enum, [zero, one, two, three]}).
-enum({explicit_enum, [{zero, 0}, {two, 2}, {four, 4}, {six, 6}]}).
-enum({mixed_enum, [zero, {one, 1}, two, {four, 4}, five, {seven, 7}]}).

%%------------------------------------------------------------------------------
%% Test functions
%%------------------------------------------------------------------------------

simple_enum_test() ->
    ?assertEqual(0, ?MODULE:to_int(simple_enum, zero)),
    ?assertEqual(1, ?MODULE:to_int(simple_enum, one)),
    ?assertEqual(2, ?MODULE:to_int(simple_enum, two)),
    ?assertEqual(3, ?MODULE:to_int(simple_enum, three)),

    ?assertEqual(zero, ?MODULE:to_atom(simple_enum, 0)),
    ?assertEqual(one, ?MODULE:to_atom(simple_enum, 1)),
    ?assertEqual(two, ?MODULE:to_atom(simple_enum, 2)),
    ?assertEqual(three, ?MODULE:to_atom(simple_enum, 3)).

explicit_enum_test() ->
    ?assertEqual(0, ?MODULE:to_int(explicit_enum, zero)),
    ?assertException(throw, bad_enum, ?MODULE:to_int(explicit_enum, one)),
    ?assertEqual(2, ?MODULE:to_int(explicit_enum, two)),
    ?assertException(throw, bad_enum, ?MODULE:to_int(explicit_enum, three)),
    ?assertEqual(4, ?MODULE:to_int(explicit_enum, four)),
    ?assertException(throw, bad_enum, ?MODULE:to_int(explicit_enum, five)),
    ?assertEqual(6, ?MODULE:to_int(explicit_enum, six)),

    ?assertEqual(zero, ?MODULE:to_atom(explicit_enum, 0)),
    ?assertException(throw, bad_enum, ?MODULE:to_atom(explicit_enum, 1)),
    ?assertEqual(two, ?MODULE:to_atom(explicit_enum, 2)),
    ?assertException(throw, bad_enum, ?MODULE:to_atom(explicit_enum, 3)),
    ?assertEqual(four, ?MODULE:to_atom(explicit_enum, 4)),
    ?assertException(throw, bad_enum, ?MODULE:to_atom(explicit_enum, 5)),
    ?assertEqual(six, ?MODULE:to_atom(explicit_enum, 6)).

mixed_enum_test() ->
    ?assertEqual(0, ?MODULE:to_int(mixed_enum, zero)),
    ?assertEqual(1, ?MODULE:to_int(mixed_enum, one)),
    ?assertEqual(2, ?MODULE:to_int(mixed_enum, two)),
    ?assertException(throw, bad_enum, ?MODULE:to_int(mixed_enum, three)),
    ?assertEqual(4, ?MODULE:to_int(mixed_enum, four)),
    ?assertEqual(5, ?MODULE:to_int(mixed_enum, five)),
    ?assertException(throw, bad_enum, ?MODULE:to_int(mixed_enum, six)),
    ?assertEqual(7, ?MODULE:to_int(mixed_enum, seven)),

    ?assertEqual(zero, ?MODULE:to_atom(mixed_enum, 0)),
    ?assertEqual(one, ?MODULE:to_atom(mixed_enum, 1)),
    ?assertEqual(two, ?MODULE:to_atom(mixed_enum, 2)),
    ?assertException(throw, bad_enum, ?MODULE:to_atom(mixed_enum, three)),
    ?assertEqual(four, ?MODULE:to_atom(mixed_enum, 4)),
    ?assertEqual(five, ?MODULE:to_atom(mixed_enum, 5)),
    ?assertException(throw, bad_enum, ?MODULE:to_atom(mixed_enum, six)),
    ?assertEqual(seven, ?MODULE:to_atom(mixed_enum, 7)).

keys_test() ->
    ?assertEqual(model([zero, one, two, three]),
                 model(?MODULE:keys(simple_enum))),
    ?assertEqual(model([zero, two, four, six]),
                 model(?MODULE:keys(explicit_enum))),
    ?assertEqual(model([zero, one, two, four, five, seven]),
		 model(?MODULE:keys(mixed_enum))),
    ?assertException(throw, bad_enum, ?MODULE:keys(undef_enum)).

values_test() ->
    ?assertEqual(model([0, 1, 2, 3]), model(?MODULE:values(simple_enum))),
    ?assertEqual(model([0, 2, 4, 6]), model(?MODULE:values(explicit_enum))),
    ?assertEqual(model([0, 1, 2, 4, 5, 7]), model(?MODULE:values(mixed_enum))),
    ?assertException(throw, bad_enum, ?MODULE:values(undef_enum)).

model(List) when is_list(List) ->
    lists:sort(List).
