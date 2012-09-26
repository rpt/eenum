## Eenum [![Build Status][travis_ci_image]][travis_ci]

**Eenum** is a simple enumeration parse transform for Erlang.
It transforms `-enum` attributes into `to_int/2` and `to_atom/2` functions.

## How to use it?

 * Run `make` to build.
 * Run `make test` to run tests.
 * Add as a dependency to your `rebar.config`:

```erlang
{deps, [{eenum, ".*", {git, "git://github.com/rpt/eenum.git"}}]}.
```

 * Look below for an example...

### Parse transform

**Eenum** uses parse transform so you have to add this to your module...

```erlang
-compile({parse_transform, eenum}).
```

... or put this inside your `rebar.config`:

```erlang
{erl_opts, [{parse_transform, eenum}]}.
```

## Example

```erlang
%% Simple enumeration
-enum({simple_enum, [zero,
                     one,
                     two,
                     three]}).

%% Explicit enumeration
-enum({explicit_enum, [{zero, 0},
                       {two, 2},
                       {four, 4},
                       {six, 6}]}).

%% Mixed enumeration
-enum({mixed_enum, [{one, 1},
                    two,
                    {four, 4},
                    five]}).
```

Will be translated to:

```erlang
-export([to_int/2, to_atom/2, keys/1, values/1]).

to_int(simple_enum, Enum) ->
    simple_enum_to_int(Enum);
to_int(explicit_enum, Enum) ->
    explicit_enum_to_int(Enum);
to_int(mixed_enum, Enum) ->
    mixed_enum_to_int(Enum);
to_int(_, _) ->
    throw(bad_enum).

simple_enum_to_int(zero) -> 0;
simple_enum_to_int(one) -> 1;
simple_enum_to_int(two) -> 2;
simple_enum_to_int(three) -> 3;
simple_enum_to_int(_) -> throw(bad_enum).

explicit_enum_to_int(zero) -> 0;
explicit_enum_to_int(two) -> 2;
explicit_enum_to_int(four) -> 4;
explicit_enum_to_int(six) -> 6;
explicit_enum_to_int(_) -> throw(bad_enum).

mixed_enum_to_int(one) -> 1;
mixed_enum_to_int(two) -> 2;
mixed_enum_to_int(four) -> 4;
mixed_enum_to_int(five) -> 5;
mixed_enum_to_int(_) -> throw(bad_enum).

to_atom(simple_enum, Enum) ->
    simple_enum_to_atom(Enum);
to_atom(explicit_enum, Enum) ->
    explicit_enum_to_atom(Enum);
to_atom(mixed_enum, Enum) ->
    mixed_enum_to_atom(Enum);
to_atom(_, _) ->
    throw(bad_enum).

simple_enum_to_atom(0) -> zero;
simple_enum_to_atom(1) -> one;
simple_enum_to_atom(2) -> two;
simple_enum_to_atom(3) -> three;
simple_enum_to_atom(_) -> throw(bad_enum).

explicit_enum_to_atom(0) -> zero;
explicit_enum_to_atom(2) -> two;
explicit_enum_to_atom(4) -> four;
explicit_enum_to_atom(6) -> six;
explicit_enum_to_atom(_) -> throw(bad_enum).

mixed_enum_to_atom(1) -> one;
mixed_enum_to_atom(2) -> two;
mixed_enum_to_atom(4) -> four;
mixed_enum_to_atom(5) -> five;
mixed_enum_to_atom(_) -> thrown(bad_enum).

keys(simple_enum) -> [zero, one, two, three];
keys(explicit_enum) -> [zero, two, four, six];
keys(mixed_enum) -> [one, two, four, five];
keys(_) -> throw(bad_enum).

values(simple_enum) -> [0, 1, 2, 3];
values(explicit_enum) -> [0, 2, 4, 6];
values(mixed_enum) -> [1, 2, 4, 5];
values(_) -> throw(bad_enum).
```

## Changes

### Version 1.1

 * Adds `keys/1` and `values/1` functions
 * Uses hierarchical functions for speedup

[travis_ci]:
http://travis-ci.org/rpt/eenum
[travis_ci_image]:
https://secure.travis-ci.org/rpt/eenum.png
