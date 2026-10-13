#lang scribble/sigplan
@(require "utils.rkt")

@title{Conclusion}

Lexical scope is a powerful organizing principle. In the context of
Racket, we believe that lexically scoped macros are the key to our
ability to define a rich ecosystem of language variants and tools,
from teaching dialects of Racket, to statically typed dialects of
Racket, to documentation languages. Phasing is a natural extension of
lexical scope that adds a new dimension to each identifier, allowing
the identifier to have different meanings in different phases while
relying on the surrounding context of an identifier to also determine
the phase in which the identifier is used.

Submodules add new expressive power to Racket in line with the
principles of lexical scope.
Besides formulating new compilation rules, much of the work
behind our design is recognizing how submodules can solve a variety of
problems for Racket programmers. We have described documentation as a
an example that relies on the full power of submodules, but simple
support for @racket[main] and @racket[test] submodules has
significantly streamlined Racket development while falling out as
natural uses of a core language feature.
