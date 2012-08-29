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
%% @doc Module for eenum parse tranform.
%% @private
-module(eenum).

%% Parse transform
-export([parse_transform/2,
	 format_error/1]).

-define(EXPORT, {attribute, 1, export, [{to_int, 2}, {to_atom, 2}]}).
-define(ERR_CLAUSE(Line, A), {clause, Line,
			      [{var, Line, '_'} || _ <- lists:seq(1, A)], [],
			      [{call, Line, {atom, Line, throw},
				[{atom, Line, bad_enum}]}]}).

%%------------------------------------------------------------------------------
%% Parse transform function
%%------------------------------------------------------------------------------

parse_transform([{attribute, 1, file, {Filename, 1}} = File,
		 Mod | Forms] = OriginalForms, _Options) ->
    put(errors, []),
    put(warnings, []),
    case find_enums(Forms, []) of
	[] ->
	    case get_warnings() of
		[] ->
		    OriginalForms;
		Warnings ->
		    {warning, OriginalForms, [{Filename, Warnings}]}
	    end;
	EnumForms ->
	    case get_errors() of
		[] ->
		    NewForms = [File, Mod, ?EXPORT |
				lists:keydelete(eof, 1, Forms)] ++ EnumForms,
		    case get_warnings() of
			[] ->
			    NewForms;
			Warnings ->
			    {warning, NewForms, [{Filename, Warnings}]}
		    end;
		Errors ->
		    {error, [{Filename, Errors}], []}
	    end
    end.

format_error({duplicate, Name}) ->
    io_lib:format("enum '~p' already defined", [Name]);
format_error({invalid, Name}) ->
    io_lib:format("invalid enum '~p'", [Name]);
format_error(invalid) ->
    io_lib:format("invalid enum", []).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

find_enums([{eof, _Line}], []) ->
    [];
find_enums([{eof, Line}], Acc) ->
    generate_funs(Line, lists:reverse(Acc));
find_enums([{attribute, Line, enum, Enums} | Rest], Acc) ->
    NewAcc = parse_enums(Enums, Line, Acc),
    find_enums(Rest, NewAcc);
find_enums([_Else | Rest], Acc) ->
    find_enums(Rest, Acc).

parse_enums({Name, Enums}, Line, Acc) when is_atom(Name) ->
    case lists:keymember(Name, 1, Acc) of
	true ->
	    add_warning({Line, {duplicate, Name}}),
	    Acc;
	false ->
	    case convert_enums(Enums, 0, []) of
		invalid ->
		    add_warning({Line, {invalid, Name}}),
		    Acc;
		ValidEnums ->
		    [{Name, ValidEnums} | Acc]
	    end
    end;
parse_enums(_, Line, Acc) ->
    add_warning({Line, invalid}),
    Acc.

convert_enums([], _, Acc) ->
    lists:reverse(Acc);
convert_enums([Atom | Rest], C, Acc) when is_atom(Atom) ->
    convert_enums(Rest, C + 1, [{Atom, C} | Acc]);
convert_enums([{Atom, Int} | Rest], C, Acc) when is_atom(Atom),
						 is_integer(Int),
						 Int >= C ->
    convert_enums(Rest, Int + 1, [{Atom, Int} | Acc]);
convert_enums(_, _, _) ->
    invalid.

generate_funs(Line, Enums) ->
    {Line2, ToIntFun} = to_int_fun(Line, Enums),
    {Line3, ToIntFuns} = enum_to_int_funs(Line2, Enums, []),
    {Line4, ToAtomFun} = to_atom_fun(Line3, Enums),
    {Line5, ToAtomFuns} = enum_to_atom_funs(Line4, Enums, []),
    [ToIntFun | ToIntFuns] ++ [ToAtomFun | ToAtomFuns] ++ [{eof, Line5 + 1}].

to_int_fun(Line, Enums) ->
    {NewLine, Clauses} = to_int_clauses(Line, Enums, []),
    Fun = {function, Line, to_int, 2, Clauses ++ [?ERR_CLAUSE(NewLine, 2)]},
    {NewLine, Fun}.

to_int_clauses(Line, [], Acc) ->
    {Line + 1, lists:reverse(Acc)};
to_int_clauses(Line, [{Name, _Enums} | Rest], Acc) ->
    Clause = {clause, Line, [{atom, Line, Name}, {var, Line, 'Enum'}], [],
	      [{call, Line, {atom, Line, enum_to_int_name(Name)},
		[{var, Line, 'Enum'}]}]},
    to_int_clauses(Line + 1, Rest, [Clause | Acc]).

enum_to_int_funs(Line, [], Acc) ->
    {Line + 1, lists:reverse(Acc)};
enum_to_int_funs(Line, [{Name, Enums} | Rest], Acc) ->
    Clauses = [{clause, Line, [{atom, Line, Atom}], [],
		[{integer, Line, Int}]} || {Atom, Int} <- Enums],
    Fun = {function, Line, enum_to_int_name(Name), 1,
	   Clauses ++ [?ERR_CLAUSE(Line, 1)]},
    enum_to_int_funs(Line + 1, Rest, [Fun | Acc]).

to_atom_fun(Line, Enums) ->
    {NewLine, Clauses} = to_atom_clauses(Line, Enums, []),
    Fun = {function, Line, to_atom, 2, Clauses ++ [?ERR_CLAUSE(NewLine, 2)]},
    {NewLine, Fun}.

to_atom_clauses(Line, [], Acc) ->
    {Line + 1, lists:reverse(Acc)};
to_atom_clauses(Line, [{Name, _Enums} | Rest], Acc) ->
    Clause = {clause, Line, [{atom, Line, Name}, {var, Line, 'Enum'}], [],
	      [{call, Line, {atom, Line, enum_to_atom_name(Name)},
		[{var, Line, 'Enum'}]}]},
    to_atom_clauses(Line + 1, Rest, [Clause | Acc]).

enum_to_atom_funs(Line, [], Acc) ->
    {Line + 1, lists:reverse(Acc)};
enum_to_atom_funs(Line, [{Name, Enums} | Rest], Acc) ->
    Clauses = [{clause, Line, [{integer, Line, Int}], [],
		[{atom, Line, Atom}]} || {Atom, Int} <- Enums],
    Fun = {function, Line, enum_to_atom_name(Name), 1,
	   Clauses ++ [?ERR_CLAUSE(Line, 1)]},
    enum_to_atom_funs(Line + 1, Rest, [Fun | Acc]).

%% Unused for now
%% add_error(Error) ->
%%     put(errors, [Error | get(errors)]).

get_errors() ->
    lists:reverse([{Line, ?MODULE, Error}
		   || {Line, Error} <- get(errors)]).

add_warning(Warning) ->
    put(warnings, [Warning | get(warnings)]).

get_warnings() ->
    lists:reverse([{Line, ?MODULE, Warning}
		   || {Line, Warning} <- get(warnings)]).

enum_to_int_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_to_int").

enum_to_atom_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_to_atom").
