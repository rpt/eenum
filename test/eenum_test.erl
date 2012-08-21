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
-module(eenum_test).

-compile({parse_transform, eenum}).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Enumerations
%%------------------------------------------------------------------------------

-enum({simple_enum, [zero, one, two, three]}).
-enum({explicit_enum, [{0, zero}, {2, two}, {4, four}, {6, six}]}).
-enum({invalid_enum, [{0, zero}, one]}).
-enum({invalid2_enum, [zero, {1, one}]}).

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

invalid_enums_test() ->
    ?assertException(error, function_clause, to_int(invalid_enum, zero)),
    ?assertException(error, function_clause, to_int(invalid_enum, one)),
    ?assertException(error, function_clause, to_atom(invalid_enum, 0)),
    ?assertException(error, function_clause, to_atom(invalid_enum, 1)),

    ?assertException(error, function_clause, to_int(invalid2_enum, zero)),
    ?assertException(error, function_clause, to_int(invalid2_enum, one)),
    ?assertException(error, function_clause, to_atom(invalid2_enum, 0)),
    ?assertException(error, function_clause, to_atom(invalid2_enum, 1)).
