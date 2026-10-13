    Title: GTP Benchmarks for Gradual Typing Performance
    Date: 2023-06-28T17:18:44
    Tags: by Ben Greenman

Sound gradual types have runtime costs.
The GTP Benchmarks have helped measure these costs since 2014.

<!-- more -->

- - -

What on earth is gradual typing performance and why does it matter?

The second question is easy to answer: performance matters because
the cost of gradual types can slow a program by several
[orders of magnitude](https://users.cs.utah.edu/~blg/publications/publications.html#tfgnvf-popl-2016).
To answer the first question, we need to step back a bit...

Normally, a type system is an ahead-of-time thing.
Programs must typecheck before they can run.
Afterwards, types can disappear.
Compiled code can safely run full-throttle and assume
that everything in its world behaves in a well-typed way.

Gradual typing leads to an entirely different situation.
It allows typed and untyped code to live together,
which means that part of the codebase might rely on type
assumptions that the rest of the codebase does not know about!

Suppose we have a typed function that averages the elements in a list:

```python
def avg(nums: list[int]):
  ....
```

This function expects inputs that have the type `list[int]`.
The typechecker will make sure that every call to `avg` in typed code
match this expectation.
But the typechecker will not check any calls to `avg` that appear in untyped
code.
How could it?
Untyped code is, by definition, untyped and un-checked!

Consequently, untyped code can ask outrageous questions when the program runs:

```
avg("hello")
avg([[1, 7], "X", 0])
avg(avg)
```

We clearly have a problem.
Typed code might contain elegant data descriptions that untyped code does not know about.
What to do?

* One option is to do nothing. Let the programmer beware that gradual
  types are unsound at the boundaries to untyped code.
  With this mindset, types remain cost-free but give zero help for debugging.
  Many languages follow this approach, including TypeScript and mypy.
* Another option is to enforce type soundness with runtime checks.
  Now we have reliable types (to some extent), but we also have costs
  to measure and minimize.

The GTP Benchmarks are a collection of 21 programs
designed to test the costs of sound gradual types.
Each program comes in two forms, untyped and typed
(written in Racket and Typed Racket), with the crucial
property that any module-by-module mix of the two forms
results in a working program.
A benchmark with `N` modules generates `2^N` partially-typed
programs that sample the space of gradual possibilities:

<img src="/img/gtp-lattice.png" width="50%" />

The table below is a birds-eye view of the benchmarks.
It lists their name, purpose, and characteristics:
whether they were originally typed (T Init),
whether they depend on untyped (U Lib)
or typed (T Lib) library code,
whether they define generative datatypes (Adapt),
and whether they send higher-order function (HOF),
polymorphic types (Poly),
recursive types (Rec),
mutable data-structure types (Mut),
immutable data-structure types (Imm),
object types (Obj),
or class types (Cls)
across any untyped boundary:

<img src="/img/gtp-programs.png" />

For more details, see [the paper](https://users.cs.utah.edu/~blg/publications/publications.html#g-rep-2023).

