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
-export([parse_transform/2]).

%%------------------------------------------------------------------------------
%% Parse transform function
%%------------------------------------------------------------------------------

%% @private
parse_transform([File, Module | Forms] = OldForms, _Options) ->
    {attribute, 1, file, {Filename, 1}} = File,
    EnumForms = find_enums(Forms, Filename, []),
    case EnumForms of
	[] ->
	    OldForms;
	_ ->
	    [File, Module, {attribute, 1, export, [{to_int,2}, {to_atom,2}]} |
	     lists:keydelete(eof, 1, Forms)] ++ EnumForms
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
find_enums([{eof, Line}], _, Acc) ->
    case Acc of
	[] ->
	    [];
	_ ->
	    generate_funs(Line, lists:reverse(Acc))
    end;
find_enums([{attribute, Line, enum, Enums} | Rest], Filename, Acc) ->
    NewAcc = parse_enums(Enums, Filename, Line, Acc),
    find_enums(Rest, Filename, NewAcc);
find_enums([_ | Rest], Filename, EnumSet) ->
    find_enums(Rest, Filename, EnumSet).

%% @private
parse_enums({Name, Enums}, Filename, Line, Acc) ->
    case lists:keymember(Name, 1, Acc) of
	true ->
	    io:format("~s:~p: enum '~p' already defined~n",
		      [Filename, Line, Name]),
	    Acc;
	false ->
	    case get_type(Enums, invalid) of
		simple ->
		    List = lists:zip(lists:seq(0, length(Enums) - 1), Enums),
		    [{Name, List} | Acc];
		explicit ->
		    [{Name, Enums} | Acc];
		invalid ->
		    io:format("~s:~p: invalid enum '~p'~n",
			      [Filename, Line, Name]),
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
