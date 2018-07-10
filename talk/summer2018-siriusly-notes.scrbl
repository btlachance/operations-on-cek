#lang scribble/base
@(require scriblib/figure)

@(define (parag . x)
   (apply elem x #:style "paragraph"))

@title{JITs and Abstract Machines@;{Not a Racket, Just a Bunch of Heuristics}}

@author{Brian LaChance}

@section{Introduction}

My work has two goals. First, to take a user's abstract machine
semantics and give them a performant implementation of it. Second, and
in support of the first, to minimize the amount of work beyond just
writing down the abstract machine.

By abstract machine I mean a small-step state transition system that
decides its next state without having to walk arbitrarily deep through
the program. So, no substitutions, no evaluation contexts, and no
structural operational semantics. (Writing a compiler for those
high-level operations efficiently is research in and of itself.)

Today's talk will be about a language to support these goals: Jam.
We'll look at a brief example (and maybe parts of a longer example)
written in Jam, and we'll also talk about progress on making the code
Jam generates performant.

@section{Example}

Let's start by taking a look at a semantics in Jam. It's for an
integer language with addition, subtraction, let, and a zero test. The
grammar for the language introduces constructed forms, terminals, and
nonterminals. The constructed forms have a fixed arity and their
definition can only refer to nonterminals. After the grammar the
nonterminals for expressions (#:control-string), environment
(#:environment), and continuation (#:continuation) are given, as well
as the initial and final states. And after all of that is the
transition function, which is defined in parts.

Each part of the transition function determines the next state of the
machine given its current state. The current state is the triple on
the left of the arrow and the next state is the triple on the right of
the arrow. For the first part, if the current state is a variable var
with some environment env and continuation k, then the next state is
one with an insignificant expression, the starting environment, and a
continuation that returns var's binding in env to k.

Think of the current state as representing this idea: we're evaluating
the expression part in a particular environment, and when we're done
evaluating the expression we do whatever the continuation says to
do. Tying this idea back to the part of the transition function we
just saw, the only thing to do after evaluating a variable is to look
up its binding in the current environment. And in general, once we
have a value, we immediately make it available to the current
continuation.

Stepping back a bit: Jam only supports abstract machines that are
similar to the CEK machine. Specifically, given definitions of
expressions, environments, and continuations, the machine's state
needs to be a triple of expressions, environments, and
continuations. Once the user's written down the syntax and the
semantics, Jam compiles those to an RPython-based interpreter. For now
there's also some small, relatively constant RPython code that has to
be manually written to tie it all together.

Within those restrictions, I've implemented a couple languages larger
than this example: a subset of Scheme (lambda, mutable variables,
call/cc, nontrivial part of the numeric tower, cons, nil,
symbols---later we'll refer to this odd subset as LC), and a semantics
for Impcore based on its big-step semantics.

@section{Performance?}

To talk about performance improvements we need to keep in mind some
notion of cost. Some costly operations RPython can automatically take
care of for us, but others require specific annotations. We'll break
the annotations down by components of the machine's state.

Our expressions are represented as objects with subexpressions stored
as fields. Since reading out of a field on an object has a cost, with
the help of the JIT we can eliminate that cost by marking the field as
immutable.

Environments are represented as objects with a lookup method, which
takes an arbitrary variable and returns a value if the receiving
environment or one in its chain binds that variable. We assume the
variables bound by a particular environment is fixed for the
environment's lifetime, which means we can arrange for those variables
to be constant. Once constant, it's easy to write constant-foldable
code that checks whether an arbitrary variable is bound in the
receiving environmnet.

When the receiving environment does bind a particular variable, it
also needs to return the corresponding value. Often enough, the values
corresponding to each variable will differ which means the entire set
for a particular environment can't be constant. But at the very least
we can store them in a structure of fixed size, and once we
communicate that to the JIT then once the environment determines the
location of a value in that structure, the JIT can compile away
everything but following that location.

Continuations are a bit harder to optimize. Unlike expressions, which
are all allocated before the program starts, a new continuation is
allocated by just about every single transition. RPython can optimize
away some of those allocations, but becuase they're allocated so often
we can't treat continuations as constant. We're left communicating to
the JIT what parts of a continuation are constant.

Each optimization on a part of the machine's state reflects
assumptions about the language. We've touched on them a little bit by
going over the optimizations, but let's be a little more explicit. For
expressions, we assume that no new expressions are created at
runtime. For environments, we assume that the variables bound in a
particular environment are fixed in number and in identity and that
they don't overlap. For continuations, we assume that the expression
stored in a continuation is fixed. There are other optimizations in
play but the ones related to the machine state are fundamental.

The assumptions are the heuristics alluded to in the title. I say
they're heuristics because they won't hold up for certain styles of
semantics or for certain languages. There are other heuristics
involved, such as deciding what things the JIT should even look at,
but the ones we've covered are enough for one day.

@section{Results}

There are a few languages written in Jam, with the two biggest being
the ones already mentioned: Impcore and the subset of Scheme, LC. In
addition to the example language we've been working through there's
also a small language based on an object calculus.

The runtime performance of LC has received the most attention, so
let's see how its performance compares to a hand-written CEK-based
interpreter, Pycket.

@figure-here["ooc-perf"
             "LC (labeled Ooc) performance relative to Pycket"
             @image["ooc-norm-20180510.pdf"]]

@Figure-ref["ooc-perf"] shows the performance of LC (labeled Ooc) on
12 of the CrossBenchmarks normalized to Pycket's runtime. The
CrossBenchmarks suite is a modified version of Larceny's
cross-platform Scheme benchmark suite, which the Pycket authors
develoepd for their ICFP 2015 publication. We used the same parameters
and infrastructure as Pycket, but to summarize: each benchmark was run
10 times and the time was measured in-system. JIT warm-up time is
included. All figures show the arithmetic mean of the 10 runs, unless
labeled otherwise.

Since the chart doesn't say how LC compares with Racket, here are the
rough relative amounts in the ICFP 2015 paper for Pycket's runtime
compared to Racket's: diviter .5X, fft .1X, fib 1X, fibc iotaX, mbrot
.1X, nqueens 1X, paraffins .5X, perm9 .5X, pnpoly .3X, simplex .5X,
sum .2X, triangl .2X. This means LC lags Racket on fib, nqueens,
paraffins, perm9, pnpoly, and simplex.

@section{Next steps}

@parag{Environments} My implementation of environments is relatively
simple compared to Pycket's. Making their optimizations applicable to
a variety of languages will need more work. For an idea of how much
environments play a role, the Pycket authors report an across the
board 3.5x slowdown with a particular optimization turned off.

