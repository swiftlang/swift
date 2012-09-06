.. _PatternMatching:

Pattern Matching
================

Elimination rules
-----------------

When type theorists consider a programming language, we break it down like this:

* What are the kinds of fundamental and derived types in the language?
* For each type, what are its introduction rules, i.e. how do you get
  values of that type?
* For each type, what are its elimination rules, i.e. how do you use
  values of that type?

Swift has a pretty small set of types right now:

* Fundamental types: currently i1, i8, i16, i32, and i64; eventually
  float and double; maybe others.
* Function types.
* Tuples. Heterogenous fixed-length products. Swift's system
  provides two basic kinds of element: positional and labelled.
* Arrays. Homogenous fixed-length aggregates.
* Algebraic data types (ADTs), introduce by oneof.  Nominal closed
  disjoint unions of heterogenous types.
* Struct types.  Nominal heterogenous fixed-length products.
* Class types.  Nominal, subtypeable heterogenous fixed-length products
  with identity.
* Protocol and protocol-composition types. Existential types restricted
  to a single variable which must also be the head type, i.e. more
  formally :math:`\exists y : P1, P2 . y`.

In addition, each of the nominal types can be made generic; this
doesn't affect the overall introduction/elimination design because an
"unapplied" generic type isn't first-class (intentionally), and an
"applied" generic type behaves essentially like a non-generic type
(also intentionally).

The point is that adding any other kind of type (e.g. SIMD vectors)
means that we need to consider its intro/elim rules.

For most of these, intro rules are just a question of picking syntax, and we
don't really need a document for that. So let's talk elimination. Generally, an
elimination rule is a way at getting back to the information the intro rule(s)
wrote into the value. So what are the specific elimination rules for these
types? How do we use them, other than in type-generic ways like passing them as
arguments to calls?

**Functions** are used by calling them. This is something of a special case:
some values of function type may carry data, there isn't really a useful model
for directly accessing it. Values of function type are basically completely
opaque.

**Scalars** are used by feeding them to primitive binary operators.  This is
also something of a special case, because there's no useful way in which scalars
can be decomposed into separate values.

**Tuples**, **structs**, and **classes** are used by projecting out
their elements.  Classes may also be turned into an object of a
supertype (which is always a class).

**Arrays** are used by projecting out slices and elements.

**Existentials** are used by performing one of the operations that the
type is known to support.

**ADTs** are used by projecting out elements of the current alternative, but how
we determine the current alternative?

Alternatives for alternatives
-----------------------------

I know of three basic designs for determining the current alternative of an ADT:

* Visitor pattern: there's some way of declaring a method on the full ADT and
  then implementing it for each individual alternative. You do this in OO
  languages mostly because there's no direct language support for closed
  disjoint unions (as opposed to open disjoint unions, which subclassing lets
  you achieve at some performance cost).

  * plus: doesn't require language support
  * plus: easy to "overload" and provide different kinds of pattern matching on
    the same type
  * plus: straightforward to add interesting ADT-specific logic, like matching a
    CallExpr instead of each of its N syntactic forms
  * plus: simple form of exhaustiveness checking
  * minus: cases are separate functions, so data and control flow is awkward
  * minus: lots of boilerplate to enable
  * minus: lots of boilerplate to use
  * minus: nested pattern matching is awful

* Query functions: dynamic_cast, dyn_cast, isa, instanceof

  * plus: easy to order and mix with other custom conditions
  * plus: low syntactic overhead for testing the alternative if you don't need
    to actually decompose
  * minus: higher syntactic overhead for decomposition

    * isa/instanceof pattern requires either a separate cast or unsafe
      operations later
    * dyn_cast pattern needs a fresh variable declaration, which is very awkward
      in complex conditions

  * minus: exhaustiveness checking is basically out the window
  * minus: some amount of boilerplate to enable

* Pattern matching

  * plus: no boilerplate to enable
  * plus: hugely reduced syntax to use if you want a full decomposition
  * plus: compiler-supported exhaustiveness checking
  * plus: nested matching is natural
  * plus: with pattern guards, natural mixing of custom conditions
  * minus: syntactic overkill to just test for a specific alternative
    (e.g. to filter it out)
  * minus: needs boilerplate to project out a common member across
    multiple/all alternatives
  * minus: awkward to group alternatives (fallthrough is a simple option
    but has issues)
  * minus: traditionally completely autogenerated by compiler and thus
    not very flexible
  * minus: usually a new grammar production that's very ambiguous with
    the expression grammar
  * minus: somewhat fragile against adding extra data to an alternative

I feel that this strongly points towards using pattern matching as the basic way
of consuming ADTs, maybe with special dispensations for querying the alternative
and projecting out common members.

Pattern matching was probably a foregone conclusion, but I wanted to spell out
that having ADTs in the language is what really forces our hand because the
alternatives are so bad. Once we need pattern-matching, it makes sense to
provide patterns for the other kinds of types as well.

Selection statement
-------------------

This is the main way we expect users to employ non-obvious pattern- matching. We
obviously need something with statement children, so this has to be a
statement. That's also fine because this kind of full pattern match is very
syntactically heavyweight, and nobody would want to embed it in the middle of an
expression. We also want a low-weight matching expression, though, for
relatively simple ADTs::

  stmt            ::= stmt-switch
  stmt-switch     ::= 'switch' expr '{' switch-group+ '}'
  switch-group    ::= case-introducer+ stmt-brace-item+
  case-introducer ::= 'case' match-pattern case-guard? ':'
  case-introducer ::= 'default' case-guard? ':'
  case-guard      ::= 'where' expr

We can get away with using "switch" here because we're going to unify
both values and patterns under match-pattern.  The works chiefly by
making decompositional binding a bit more awkward, but has the major
upside of reducing the likelihood of dumb mistakes (rebinding 'true',
for example), and it means that C-looking switches actually match our
semantics quite closely.

I keep going back and forth about having a "default" case-introducer.
On the one hand, I kindof want to encourage total matches;  on the
other hand, having it is consistent with C, and it's not an unnatural
style.  We can certainly recommend complete matches in switches,
though.

Despite the lack of grouping braces, the semantics are that the statements in
each case-group form their own scope, and falling off the end causes control to
resume at the end of the match statement — i.e. "implicit break", not "implicit
fallthrough".

Chris seems motivated to eventually add an explicit 'fallthrough'
statement. If we did this, my preference would be to generalize it by
allowing the match to be reperformed with a new value, e.g.
:code:`fallthrough(something)`, at least optionally.  I think having
local functions removes a lot of the impetus, but not so much as to
render the feature worthless.

Syntactically, braces and the choice of case keywords are all bound
together. The thinking goes as follows. In Swift, statement scopes are always
grouped by braces. It's natural to group the cases with braces as well. Doing
both lets us avoid a 'case' keyword, but otherwise it leads to ugly style,
because either the last case ends in two braces on the same line or cases have
to further indented. Okay, it's easy enough to not require braces on the match,
with the grammar saying that cases are just greedily consumed — there's no
ambiguity here because the match statement is necessarily within braces. But
that leaves the code without a definitive end to the cases, and the closing
braces end up causing a lot of unnecessary vertical whitespace, like so::

  switch (x)
  case .foo {
    …
  }
  case .bar {
    …
  }
  
So instead, let's require the match statement to have braces, and
we'll allow the cases to be written without them::

  switch (x) {
  case .foo:
    …
  case .bar:
    …
  }

That's really a lot prettier, except it breaks the rule about always grouping
scopes with braces (we *definitely* want different cases to establish different
scopes). Something has to give, though.

We require the trailing colon because it's a huge cue for separating
things, really making single-line cases visually appealing, and the
fact that it doesn't suggest closing punctuation is a huge boon.  It's
also directly precedented in C, and it's even roughly the right
grammatical function.  It used to look pretty silly after ":name", but
it's way better after ".name".

The semantics of a switch statement are to first evaluate the value
operand, then proceed down the list of case-introducers and execute
the statements for the switch-group that had the first satisfied
introducer.  It is an error if a case-pattern can never trigger
because the earlier case-patterns are exhaustive.

A 'default' is satisfied if it has no guard or if the guard evaluates to true.

A 'case' is satisfied if the pattern is satisfied and, if there's a guard,
the guard evaluates to true after binding variables.  The guard is not
evaluated if the pattern is not fully satisfied.  We'll talk about satisfying
a pattern later.

All of the case-patterns in a case-group must bind exactly the same variables
with exactly the same types.

Since falling out of a statement is reasonable behavior in an
imperative language — in contrast to, say, a functional language where
you're in an expression and you need to produce a value — there's a
colorable argument that non-exhaustive matches should be okay.  I'm
inclined to disagree, though, and say that they should be errors and
people who want non-exhaustive matches can put in default cases.
Exhaustiveness actually isn't that difficult to check, at least over
ADTs.  It's also really the behavior that I would expect from the
syntax, or at least implicitly falling out seems dangerous in a way
that nonexhaustive checking doesn't.  The complications with checking
exhaustiveness are pattern guards and matching expressions. The
obvious conservatively-safe rule is to say "ignore cases with pattern
guards or matching expressions during exhaustiveness checking", but
some people really want to write "where x < 10" and "where x >= 10",
and I can see their point. At the same time, we really don't want to
go down that road.

Patterns come up (or potentially come up) in a few other places in the grammar::

Var bindings
------------

Variable bindings only have a single pattern, which has to be exhaustive, which
also means there's no point in supporting guards here. I think we just get
this::

  decl-var ::= 'var' attribute-list? pattern-exhaustive value-specifier

Function parameters
-------------------

The functional languages all permit you to directly pattern-match in the
function declaration, like this example from SML::

  fun length nil = 0
    | length (a::b) = 1 + length b

This is really convenient, but there's probably no reasonable analogue in
Swift. One specific reason: we want functions to be callable with keyword
arguments, but if you don't give all the parameters their own names, that won't
work.

The current Swift approximation is::

  func length(list : List) : Int {
    switch list {
      case .nil: return 0
      case .cons(_,?tail): return 1 + length(tail)
    }
  }

That's quite a bit more syntax, but it's mostly the extra braces from the
function body. We could remove those with something like this::

  func length(list : List) : Int = switch list {
    case .nil: return 0
    case .cons(_,?tail): return 1 + length(tail)
  }

Anyway, that's easy to add later if we see the need.

Assignment
----------

This is a bit iffy. It's a lot like var bindings, but it doesn't have a keyword,
so it's really kindof ambiguous given the pattern grammar.

Also, l-value patterns are weird. I can come up with semantics for this, but I
don't know what the neighbors will think::

  var perimeter : double
  .feet(x) += yard.dimensions.height // returns Feet, which has one constructor, :feet.
  .feet(x) += yard.dimensions.width

It's probably better to just have l-value tuple expressions and not work in
arbitrary patterns.

Pattern-match expression
------------------------

This is an attempt to provide that dispensation for query functions we were
talking about.

I think this should bind looser than any binary operators except assignments;
effectively we should have::

  expr-binary ::= # most of the current expr grammar
  
  expr ::= expr-binary
  expr ::= expr-binary 'matches' case-pattern pattern-guard?

The semantics are that this evaluates to true if the pattern and pattern-guard
are satisfied. If the pattern binds variables, it's an error if the expression
isn't immediately used as a condition; otherwise, the variables are in scope in
any code dominated by the 'true' edge. I've intentionally written this in a way
that suggests it holds even within complex expressions, but at the very least
this should work::

  if rect.dimensions matches (height = h, width = w) where h >= w {
    …
  }

The keyword 'matches' is not set in stone. It's hardly even set in sand. Clearly
we should use =~. :)

Pattern grammar
---------------

The standard syntax rule is that the pattern grammar mirrors the
introduction-rule expression grammar, but parsing a patterns whenever
you would otherwise put an expression.  This means that, for example,
if we add array literal expressions, we should also add a
corresponding array literal pattern. I think that principle is worth
keeping.

We're blurring the distinction between patterns and expressions a lot
here.  My current thinking is that this simplifies things for the
programmer --- the user concept becomes basically "check whether we're
equal to this expression, but allow some holes".  But it's possible
that it instead might be really badly confusing.  We'll see!  It'll be fun!

This kindof forces us to have parallel pattern grammars for the two
major clients::

  * Match patterns are used in :code:`switch` and :code:`matches`, where
    we're decomposing something with a real possibility of failing.
    This means that expressions are okay in leaf positions, but that
    name-bindings need to be explicitly advertised in some way to
    reasonably disambiguate them from expressions.
  * Exhaustive patterns are used in :code:`var` declarations
    and function signatures.  They're not allowed to be non-exhaustive,
    so having a match expression doesn't make any sense.  Name bindings
    are common and so shouldn't be penalized.

You might think that having a "pattern" as basic as :code:`foo` mean
something different in two different contexts would be confusing, but
actually I don't think people will generally think of these as the
same production — you might if you were in a functional language where
you really can decompose in a function signature, but we don't allow
that, and I think that will serve to divide them in programmers' minds.
So we can get away with some things. :)

In general, a lot of these productions are the same, so I'm going to
talk about *-patterns, with some specific special rules that only
apply to specific pattern kinds.

  *-pattern ::= '_'

A single-underscore identifier is always an "ignore" pattern.  It
matches anything, but does not bind it to a variable.

  exhaustive-pattern ::= identifier
  match-pattern ::= '?' identifier

Any more complicated identifier is a variable-binding pattern.  It is
illegal to bind the same identifier multiple times within a pattern.
However, the variable does come into scope immediately, so in a match
pattern you can have a latter expression which refers to an
already-bound variable.  I'm comfortable with constraining this to
only work "conveniently" left-to-right and requiring more complicated
matches to use guard expressions.

In a match pattern, variable bindings must be prefixed with a ? to
disambiguate them from an expression consisting of a variable
reference.  I considered using 'var' instead, but using punctuation
means we don't need a space, which means this is much more compact in
practice.

  exhaustive-pattern ::= exhaustive-pattern ':' type

In an exhaustive pattern, you can annotate an arbitrary sub-pattern
with a type.  This is useful in an exhaustive pattern: the type of a
variable isn't always inferrable (or correctly inferrable), and types
in function signatures are generally outright required.  It's not as
useful in a match pattern, and the colon can be grammatically awkward
there, so we disallow it.

  match-pattern ::= match-pattern-identifier match-pattern-tuple?
  match-pattern-identifier ::= '.' identifier
  match-pattern-identifier ::= match-pattern-identifier-tower
  match-pattern-identifier-tower ::= identifier
  match-pattern-identifier-tower ::= identifier
  match-pattern-identifier-tower ::= match-pattern-identifier-tower '.' identifier

A match pattern can resemble a global name or a call to a global name.
As a call, the match-pattern-tuple must begin with an unspaced '('.
The global name is resolved as normal, and then the pattern is
interpreted according to what is found:

- If the name resolves to a type, then for the pattern to be
  satisfied, the dynamic type of the matched value must be the
  specified type (or a subtype).  If this is trivially impossible
  (e.g., if the matched expression is not existential and the static
  types are unrelated), the pattern is ill-formed.

  In addition, if there is an arguments clause, it must be
  non-empty, and each element in the clause must have an identifier.
  For each element, the identifier must correspond to a known
  property of the type, and the value of that property must satisfy
  the element pattern.

- If the name resolves to a oneof element, then the dynamic type
  of the matched value must match the oneof type as discussed above,
  and the value must be of the specified element.  There must be
  an arguments clause if and only if the element has a value type.
  If so, the value of the element is matched against the clause
  pattern.

- Otherwise, the argument clause (if present) must also be
  syntactically valid as an expression, and the entire pattern is
  reinterpreted as an expression.

This is all a bit lookup-sensitive, which makes me uncomfortable, but
otherwise I think it makes for attractive syntax.  I'm also a little
worried about the way that, say, :code:`f(x)` is always an expression
but :code:`A(x)` is a pattern.  Requiring property names when matching
properties goes some way towards making that okay.

I'm not totally sold on not allowing positional matching against
struct elements; that seems unfortunate in cases where positionality
is conventionally unambiguous, like with a point.

  match-pattern ::= expression

When ambiguous, match patterns are interpreted using a
pattern-specific production.  I believe it should be true that, in
general, match patterns for a production accept a strict superset of
valid expressions, so that (e.g.) we do not need to disambiguate
whether an open paren starts a tuple expression or a tuple pattern,
but can instaed just aggressively parse as a pattern.  Note that
binary operators can mean that, using this strategy, we sometimes have
to retroactively rewrite a pattern as an expression.

It's always possible to disambiguate something as an expression by
doing something not allowing in patterns, like using a unary operator
or calling an identity function; those seem like unfortunate language
solutions, though.

A value satisfies an expression pattern if it equals the value of the
expression.  I guess this means the == operator, but maybe it means
calling some other global or member function instead.

I'd like to keep the order of evaluation and testing of expressions
within a pattern unspecified if I can; I imagine that there should be
a lot of cases where we can rule out a case using a cheap test instead
of a more expensive one, and it would suck to have to run the
expensive one just to have cleaner formal semantics.  Specifically,
I'm worried about cases like :code:`case [foo(), 0]:`; if we can test
against 0 before calling :code:`foo()`, that would be great.  Also, if
a name is bound and then used directly as an expression later on, it
would be nice to have some flexibility about which value is actually
copied into the variable, but this is less critical.

  *-pattern ::= *-pattern-tuple
  *-pattern-tuple ::= '(' *-pattern-tuple-element-list? '...'? ')'
  *-pattern-tuple-element-list ::= *-pattern-tuple-element
  *-pattern-tuple-element-list ::= *-pattern-tuple-element ',' pattern-tuple-element-list
  *-pattern-tuple-element ::= *-pattern
  *-pattern-tuple-element ::= identifier '=' *-pattern

Tuples are interesting because of the labelled / non-labelled
distinction. Especially with labelled elements, it is really nice to
be able to ignore all the elements you don't care about. This grammar
permits some prefix or set of labels to be matched and the rest to be
ignored.

Miscellaneous
-------------

It would be interesting to allow overloading / customization of
pattern-matching. We may find ourselves needing to do something like this to
support non-fragile pattern matching anyway (if there's some set of restrictions
that make it reasonable to permit that). The obvious idea of compiling into the
visitor pattern is a bit compelling, although control flow would be tricky —
we'd probably need the generated code to throw an exception. Alternatively, we
could let the non-fragile type convert itself into a fragile type for purposes
of pattern matching.

If we ever allow infix ADT constructors, we'll need to allow them in patterns as
well.

Eventually, we will build regular expressions into the language, and we will
allow them directly as patterns and even bind grouping expressions into user
variables.

John.
