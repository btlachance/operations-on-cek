#lang scribble/doc
@(require scriblib/footnote
          scribble/manual)
@(define-footnote TODO TODO-part)

@title{Compiling CEK machine specifications to RPython}
@author+email["Brian Lachance" "blachance@gmail.com"]

@section{A metalanguage for CEK machines}

CEK machines are simple but powerful: they have straightforward
specifications and they scale up to specifying languages with complex
effects. Interpreting these machines in a metalanguage with pattern
matching is straightforward, too. For the call-by-value lambda
calculus, its CEK machine yields a simple interpreter. Below are two
interpreters written in the style of a CEK machine, one in Racket and
one in SML.

@;; TODO: include my CEK example but without its test submodule
@;; TODO: implement and include a similar interpreter in SML

But programmers need more tools than just lambda. The success of the
Pycket project strongly suggests that, with Pypy's metatracing
framework, interpeters for functional programming languages can be
implemented in a CEK machine-style while still competing with mature
implementations on performance.

We would like to investigate how well this combination, CEK
machine-style interpreters and Pypy's metatracing framework, scales to
implementations of other programming languages. To this end, we are
developing a metalanguage called metaCEK for specifying CEK machines
that can be translated to RPython. If the results are positive,
programming language implementers then only have to formulate their
language in the style of a well-known abstract machine to implement a
competitive JITted interpreter.

@section{Primitive operations}

Programming languages typically offer programmers a variety of
primitive operations. There are two ways to add these to a language
specified with metaCEK: either use built-in primitives or provide
RPython code that implements the desired primitives.

If a primitive operation yields control back to the object language
before it completes, then it must handle the machine state carefully.
For example, if the primitive calls functions in the object language,
it has to make sure it adheres to the object language's calling
convention @TODO{The Pycket papers mention this issue explicitly which
is where I first saw this mentioned. I don't know if it's common
knowledge and thus does not warrant an additional reference, e.g.
people have to do something similar when adding such a primitive to a
CPS' language, I just don't remember someone spelling it out like they
did.}

Because metaCEK assumes functions are implemented in terms of lambda,
the built-in primitives all adhere to the calling convention.
@TODO{What if the language doesn't use lambda at all? What about a
Forth-like, are those not appropriate for this framework? Is lambda
really the issue here?}

@section{Binding}

The metalanguage uses Racket's built-in identifiers as much as
possible. Referring to the `adder` example, the identifiers e, n, v,
env, and k are all in binding positions for the metalanguage.
Specifically, the left-hand side of a ::= is a binding position. The
identifier add1, though, is not in a binding position for the
metalanguage. But, it is considered bound in the object language; how
else would the object language know it has an addition operation?

These questions bring up an important question about this language:
what is the relation between an identifier in the metalanguage and
identifiers that occur in the object language's RPython-based
implementation? on the interpreter?

It seems like the bound identifiers in the metalanguage should have no
residual in the object language. The free identifiers in the
metalanguage, though, are bound in a different way: they should be
used consistently throughout the implementation, but we'd like if
Racket knew that occurrences of add1 were all the same. This way, we
can use Racket's built-in tools to rename add1 to s if we were feeling
particularly historic.

@section{Compiling}

As the Racket and SML examples above showed, pattern matching makes
for simple CEK machine-style interpreters. Specifically, it makes it
easy to dispatch on any combination of the machine's registers.

Thinking about how we will generate RPython code, it seems appropriate
to do this in two steps: define some general AST class that has an
interpret method and make each syntactic form a subclass of it that
overrides the interpret method. The Pycket project does this @TODO{Is
this how all RPython interpreters are written? or is this just a
design decision the Pycket folks made?} and it fits the
object-oriented nature of RPython.

The four rules that define the CEK machine's step relation then get
compiled as follows: the first rule is implemented as the interpret
method of an App class, the second and third rules are implemented as
the interpret method of a Value class (which does the case analysis to
distinguish between the two rules), and the fourth rule is implemented
as the interpret method of a Var class. @TODO{If all object-language
values have to be subclasses of our AST in RPython, will this incur
unnecessary boxing/dispatch overheads?  Even pycket boxes a lot of
values at the metalevel (e.g. no 31-bit signed integers), but they
don't appear to be subclasses of AST.}

@section{Restrictions}

Currently, we require the expression position of every step to
syntactically correspond to a single form. Because the example below
refers to @code{e} in the expression position, we reject it. @TODO{Do we
have to have this restriction? Right now, it's becuase we want to be
able to compile each form to a class in RPython. Is that even
necessary? It's what Pycket does, but what are the alternatives and
tradeoffs to other compilation styles?}

@codeblock{
 (define-cek
   #:expression
   (e ::= n (add1 e))
   #:env ...
   #:continuation ...
   #:step
   [(e env k) --> (,(println (term e)) env k)])
}

@(TODO-part)
