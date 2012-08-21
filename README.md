Eenum
=====

[![Build Status][travis_ci_image]][travis_ci]

**Eenum** is a simple enumeration parse tranform for Erlang. It transforms `-enum` attributes into `to_int/2` and `to_atom/2` functions.

How to use it?
--------------

 * Run `make` to build.
 * Run `make test` to run tests.
 * Add as a dependency to your `rebar.config`:

```erlang
{deps, [{eenum, ".*", {git, "git://github.com/rpt/eenum.git"}}]}.
```

### Parse tranform

**Eenum** uses parse transform so you have to add this to your module...

```erlang
-compile({parse_transform, eenum}).
```

... or put this inside your `rebar.config`:

```erlang
{erl_opts, [{parse_transform, eenum}]}.
```

### Simple enumerations

```erlang
-enum({simple_enum, [zero,
                     one,
                     two,
                     three]}).
```

Will be translated to:

```erlang
-export([to_int/2, to_atom/2]).

to_int(simple_enum, zero) -> 0;
to_int(simple_enum, one) -> 1;
to_int(simple_enum, two) -> 2;
to_int(simple_enum, three) -> 3.

to_atom(simple_enum, 0) -> zero;
to_atom(simple_enum, 1) -> one;
to_atom(simple_enum, 2) -> two;
to_atom(simple_enum, 3) -> three.
```

### Explicit enumerations

```erlang
-enum({explicit_enum, [{0, zero},
                       {2, two},
                       {4, four},
                       {6, six}]}).
```

Will be translated to:

```erlang
-export([to_int/2, to_atom/2]).

to_int(explicit_enum, zero) -> 0;
to_int(explicit_enum, two) -> 2;
to_int(explicit_enum, four) -> 4;
to_int(explicit_enum, six) -> 6.

to_atom(explicit_enum, 0) -> zero;
to_atom(explicit_enum, 2) -> two;
to_atom(explicit_enum, 4) -> four;
to_atom(explicit_enum, 6) -> six.
```

[travis_ci]:
http://travis-ci.org/rpt/eenum
[travis_ci_image]:
https://secure.travis-ci.org/rpt/eenum.png
