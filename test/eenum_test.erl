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
-module(eenum_test).

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
    ?assertEqual(0, to_int(simple_enum, zero)),
    ?assertEqual(1, to_int(simple_enum, one)),
    ?assertEqual(2, to_int(simple_enum, two)),
    ?assertEqual(3, to_int(simple_enum, three)),

    ?assertEqual(zero, to_atom(simple_enum, 0)),
    ?assertEqual(one, to_atom(simple_enum, 1)),
    ?assertEqual(two, to_atom(simple_enum, 2)),
    ?assertEqual(three, to_atom(simple_enum, 3)).

explicit_enum_test() ->
    ?assertEqual(0, to_int(explicit_enum, zero)),
    ?assertException(error, function_clause, to_int(explicit_enum, one)),
    ?assertEqual(2, to_int(explicit_enum, two)),
    ?assertException(error, function_clause, to_int(explicit_enum, three)),
    ?assertEqual(4, to_int(explicit_enum, four)),
    ?assertException(error, function_clause, to_int(explicit_enum, five)),
    ?assertEqual(6, to_int(explicit_enum, six)),

    ?assertEqual(zero, to_atom(explicit_enum, 0)),
    ?assertException(error, function_clause, to_atom(explicit_enum, 1)),
    ?assertEqual(two, to_atom(explicit_enum, 2)),
    ?assertException(error, function_clause, to_atom(explicit_enum, 3)),
    ?assertEqual(four, to_atom(explicit_enum, 4)),
    ?assertException(error, function_clause, to_atom(explicit_enum, 5)),
    ?assertEqual(six, to_atom(explicit_enum, 6)).

mixed_enum_test() ->
    ?assertEqual(0, to_int(mixed_enum, zero)),
    ?assertEqual(1, to_int(mixed_enum, one)),
    ?assertEqual(2, to_int(mixed_enum, two)),
    ?assertException(error, function_clause, to_int(mixed_enum, three)),
    ?assertEqual(4, to_int(mixed_enum, four)),
    ?assertEqual(5, to_int(mixed_enum, five)),
    ?assertException(error, function_clause, to_int(mixed_enum, six)),
    ?assertEqual(7, to_int(mixed_enum, seven)),

    ?assertEqual(zero, to_atom(mixed_enum, 0)),
    ?assertEqual(one, to_atom(mixed_enum, 1)),
    ?assertEqual(two, to_atom(mixed_enum, 2)),
    ?assertException(error, function_clause, to_atom(mixed_enum, three)),
    ?assertEqual(four, to_atom(mixed_enum, 4)),
    ?assertEqual(five, to_atom(mixed_enum, 5)),
    ?assertException(error, function_clause, to_atom(mixed_enum, six)),
    ?assertEqual(seven, to_atom(mixed_enum, 7)).
