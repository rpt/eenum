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
-module(eenum).

%% Parse transform
-export([parse_transform/2,
	 format_error/1]).

-define(EXPORT, {attribute, 1, export, [{to_int, 2}, {to_atom, 2}]}).

%%------------------------------------------------------------------------------
%% Parse transform function
%%------------------------------------------------------------------------------

%% @private
parse_transform([File, Module | Forms] = OriginalForms, _Options) ->
    {attribute, 1, file, {Filename, 1}} = File,
    put(errors, []),
    put(warnings, []),
    case find_enums(Forms, []) of
	[] ->
	    OriginalForms;
	EnumForms ->
	    case get_errors() of
		[] ->
		    NewForms = [File, Module, ?EXPORT |
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

%% @private
format_error({duplicate, Name}) ->
    io_lib:format("enum '~p' already defined", [Name]);
format_error({invalid, Name}) ->
    io_lib:format("invalid enum '~p'", [Name]).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
find_enums([{eof, _Line}], []) ->
    [];
find_enums([{eof, Line}], Acc) ->
    generate_funs(Line, lists:reverse(Acc));
find_enums([{attribute, Line, enum, Enums} | Rest], Acc) ->
    NewAcc = parse_enums(Enums, Line, Acc),
    find_enums(Rest, NewAcc);
find_enums([_Else | Rest], Acc) ->
    find_enums(Rest, Acc).

%% @private
parse_enums({Name, Enums}, Line, Acc) when is_atom(Name) ->
    case lists:keymember(Name, 1, Acc) of
	true ->
	    add_warning({Line, {duplicate, Name}}),
	    Acc;
	false ->
	    case get_type(Enums, invalid) of
		simple ->
		    List = lists:zip(lists:seq(0, length(Enums) - 1), Enums),
		    [{Name, List} | Acc];
		explicit ->
		    [{Name, Enums} | Acc];
		invalid ->
		    add_warning({Line, {invalid, Name}}),
		    Acc
	    end
    end.

%% @private
get_type([], Type) ->
    Type;
get_type([Atom | Rest], invalid) when is_atom(Atom) ->
    get_type(Rest, simple);
get_type([Atom | Rest], simple) when is_atom(Atom) ->
    get_type(Rest, simple);
get_type([Atom | _], explicit) when is_atom(Atom) ->
    invalid;
get_type([{Int, Atom} | Rest], invalid) when is_integer(Int),
					     is_atom(Atom) ->
    get_type(Rest, explicit);
get_type([{Int, Atom} | Rest], explicit) when is_integer(Int),
					      is_atom(Atom) ->
    get_type(Rest, explicit);
get_type([{Int, Atom} | _], simple) when is_integer(Int),
					      is_atom(Atom) ->
    invalid;
get_type(_, _) ->
    invalid.

%% @private
generate_funs(Line, Enums) ->
    {Line2, ToIntFun} = to_int_fun(Line, Enums),
    {Line3, ToAtomFun} = to_atom_fun(Line2, Enums),
    [ToIntFun, ToAtomFun, {eof, Line3}].

%% @private
to_int_fun(Line, Enums) ->
    {NewLine, Clauses} = to_int_clauses(Line, Enums, []),
    Fun = {function, Line, to_int, 2, Clauses},
    {NewLine, Fun}.

%% @private
to_int_clauses(Line, [], Acc) ->
    {Line + 1, Acc};
to_int_clauses(Line, [{Name, Enums} | Rest], Acc) ->
    Clauses = [{clause, Line, [{atom, Line, Name},
			       {atom, Line, Atom}],
		[], [{integer, Line, Int}]} || {Int, Atom} <- Enums],
    to_int_clauses(Line + 1, Rest, Acc ++ Clauses).

%% @private
to_atom_fun(Line, Enums) ->
    {NewLine, Clauses} = to_atom_clauses(Line, Enums, []),
    Fun = {function, Line, to_atom, 2, Clauses},
    {NewLine, Fun}.

%% @private
to_atom_clauses(Line, [], Acc) ->
    {Line + 1, Acc};
to_atom_clauses(Line, [{Name, Enums} | Rest], Acc) ->
    Clauses = [{clause, Line, [{atom, Line, Name},
			       {integer, Line, Int}],
		[], [{atom, Line, Atom}]} || {Int, Atom} <- Enums],
    to_atom_clauses(Line + 1, Rest, Acc ++ Clauses).

%% Unused for now
%% @private
%% add_error(Error) ->
%%     put(errors, [Error | get(errors)]).

%% @private
get_errors() ->
    [{Line, ?MODULE, Error} || {Line, Error} <- get(errors)].

%% @private
add_warning(Warning) ->
    put(warnings, [Warning | get(warnings)]).

%% @private
get_warnings() ->
    [{Line, ?MODULE, Warning} || {Line, Warning} <- get(warnings)].
