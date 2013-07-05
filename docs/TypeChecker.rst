.. @raise litre.TestsAreMissing

Type Checker Design and Implementation
========================================

Purpose
-----------------

This document describes the design and implementation of the Swift type checker. It is intended for developers who wish to modify, extend, or improve on the type checker, or simplify to understand in greater depth how the Swift type system works. Familiarity with the Swift programming language is assumed.

Approach
-------------------

The Swift language and its type system incorporate a number of popular language features, including object-oriented programming via classes, function and operator overloading, subtyping, and constrained parametric polymorphism. Swift makes extensive use of type inference, allowing one to omit the types of many variables and expressions. For example::

  func round(x : Double) -> Int { /* ... */ }
  var pi : Double = 3.14159
  var three = round(pi) // 'three' has type 'Int'

  func identity<T>(x : T) -> T { return x }
  var eFloat : Float = -identity(2.71828)  // numeric literal gets type 'Float'

Swift's type inference allows type information to flow in two directions. As in most mainstream languages, type information can flow from the leaves of the expression tree (e.g., the expression 'pi', which refers to a double) up to the root (the type of the variable 'three'). However, Swift also allows type information to flow from the context (e.g., the fixed type of the variable 'eFloat') at the root of the expression tree down to the leaves (the type of the numeric literal 2.71828). This bi-directional type inference is common in languages that use ML-like type systems, but is not present in mainstream languages like C++, Java, C#, or Objective-C.

Swift implements bi-directional type inference using a constraint-based type checker that is reminiscent of the classical Hindley-Milner type inference algorithm. The use of a constraint system allows a straightforward, general presentation of language semantics that is decoupled from the actual implementation of the solver. It is expected that the constraints themselves will be relatively stable, while the solver will evolve over time to improve performance and diagnostics.

The Swift language contains a number of features not part of the Hindley-Milner type system, including constrained polymorphic types and function overloading, which complicate the presentation and implementation somewhat. On the other hand, Swift limits the scope of type inference to a single expression or statement, for purely practical reasons: we expect that we can provide better performance and vastly better diagnostics when the problem is limited in scope.

Type checking proceeds in three main stages:

`Constraint Generation`_
  Given an input expression and (possibly) additional contextual
  information, generate a set of type constraints that describes the
  relationships among the types of the various subexpressions. The
  generated constraints may contain unknown types, represented by type
  variables, which will be determined by the solver.

`Constraint Solving`_
  Solve the system of constraints by assigning concrete types to each
  of the type variables in the constraint system. The constraint
  solver should provide the most specific solution possible among
  different alternatives.

`Solution Application`_
  Given the input expression, the set of type constraints generated
  from that expression, and the set of assignments of concrete types
  to each of the type variables, produce a well-typed expression that
  makes all implicit conversions (and other transformations) explicit
  and resolves all unknown types and overloads. This step cannot fail.

The following sections describe these three stages of type checking,
as well as matters of performance and diagnostics.

Constraints
----------------
A constraint system consists of a set of type constraints. Each type
constraint either places a requirement on a single type (e.g., it is
an integer literal type) or relates two types (e.g., one is a subtype
of the other). The types described in constraints can be any type in
the Swift type system including, e.g., builtin types, tuple types,
function types, oneof/struct/class types, protocol types, and generic
types. Additionally, a type can be a type variable ``T`` (which are
typically numbered, ``T0``, ``T1``, ``T2``, etc., and are introduced
as needed), and type variables can be used in place of any other type,
e.g., a tuple type ``(T0, Int, (T0) -> Int)`` involving the type
variable ``T0``.

There are a number of different kinds of constraints used to describe
the Swift type system:

**Equality**
  An equality constraint requires two types to be identical. For
  example, the constraint ``T0 == T1`` effectively ensures that ``T0`` and
  ``T1`` get the same concrete type binding. There are three different
  flavors of equality constraints: 

    -  Exact equality constraints, or  "binding", written ``T0 := X``
       for some type variable ``T0`` and  type ``X``, which requires
       that ``T0`` be exactly identical to ``X``;
    - Equality constraints, written ``X == Y`` for types ``X`` and ``Y``, which require ``X`` and ``Y`` to have the same type, ignoring lvalue types in the process. For example, the constraint ``T0 == X`` would be satisfied by assigning ``T0`` the type ``X`` and by assigning ``T0`` the type ``[byref] X``.
    - Rvalue equality constraints, written ``T0 ==R X``, which specify that ``T0`` be the same as the rvalue type of ``X``. The rvalue type of a type strips off the ``[byref]``, if present, and looks at the underlying object type. 

**Subtyping**
  A subtype constraint requires the first type to be equivalent to or
  a subtype of the second. For example, a class type ``Dog`` is a
  subtype of a class type ``Animal`` if ``Dog`` inherits from
  ``Animal`` either directly or indirectly. There are two forms of
  subtyping constraint:

    - Normal subtyping, written ``X < Y``, which requires that ``X`` be a subtype of ``Y``, and
    - Trivial subtyping, written ``X <t Y``, which requires that ``X`` be a subtype of ``Y`` and that the runtime representation of an instance of type ``X`` be substitutable for the runtime representation of an instance of type ``Y``.

**Conversion**
  A conversion constraint requires that the first type be convertible
  to the second, which includes subtyping and equality. Additionally,
  it allows a user-defined conversion function to be
  called. Conversion constraints are written ``X <c Y``, read as
  ```X`` can be converted to ``Y```.

**Construction**
  A construction constraint, written ``X <C Y`` requires either that
  the first type can be convertible to the second type or that the
  second type be a nominal type with a constructor that accepts a
  value of the first type. For example, the constraint``Int <C
  String`` is satisfiable because ``String`` has a constructor that
  accepts an ``Int``.

**Literal**
  A literal constraint requires that the constrained type conform to
  the (informal) protocol required to type a particular literal and
  create an instance of the constrained type. There are literal
  constraint flavors for each kind of literal, e.g., integer literal
  constraints, floating literal constraints, character literal
  constraints,  and string literal constraints.

**Member**
  A member constraint ``X[.name] == Y`` specifies that the first type
  (``X``) have a member (or an overloaded set of members) with the
  given name, and that the type of that member be bound to the second
  type (``Y``).  There are two flavors of member constraint: value
  member constraints, which refer to the member in an expression
  context, and type member constraints, which refer to the member in a
  type context (and therefore can only refer to types).

**Archetype**
  An archetype constraint requires that the constrained type be bound
  to an archetype. This is a very specific kind of constraint that is
  only used for calls to operators in protocols.

Constraint Generation
``````````````````````````
The process of constraint generation produces a constraint system
that relates the types of the various subexpressions within an
expression. Programmatically, constraint generation walks an
expression from the leaves up to the root, assigning a type (which
often involves type  variables) to each subexpression as it goes. 

Constraint generation is driven by the syntax of the
expression, and each different kind of expression---function
application, member access, etc.---generates a specific set of
constraints. Here, we enumerate the primary expression kinds in the
language and describe both the type assigned to the expression and the
constraints generated from such as expression. We use ``T(a)`` to
refer to the type assigned to the subexpression ``a``. The constraints
and types generated from the primary expression kinds are:

**Declaration reference**
  An expression that refers to a declaration ``x`` is assigned the
  type of a reference to ``x``. For example, if ``x`` is declared as
  ``var x : Int``, the expression ``x`` is assigned the type
  ``[byref(implicit)] Int``. No constraints are generated.

  When a name refers to a set of overloaded declarations, the
  selection of the appropriate declaration is handled by the
  solver. This particular issue is discussed in the `Overloading`_
  section. Additionally, when the name refers to a generic function or
  a generic type, the declaration reference may introduce new type
  variables; see the `Polymorphic Types`_ section for more information.
 
**Member reference**
  A member reference expression ``a.b`` is assigned the type ``T0``
  for a fresh type variable ``T0``. In addition, the expression
  generates the value member constraint ``T(a).b == T0``.  Member
  references may end up resolving to a member of a nominal type or an
  element of a element; in the latter case, the name (``b``) may
  either be an identifier or a positional argument (e.g., ``$1``).

  Note that resolution of the member constraint can refer to a set of
  overloaded declarations; this is described further in the
  `Overloading`_ section.

**Unresolved member reference**
  An unresolved member reference ``.name`` refers to a member of a
  oneof type. The oneof type is assumed to have a fresh variable
  type``T0`` (since that type can only be known from context), and a
  value member constraint ``T0.name == T1``, for fresh type variable
  ``T1``, captures the fact that it has a member named ``name`` with
  some as-yet-unknown type ``T1``. The type of the unresolved member
  reference is ``T1``, the type of the member. 

  Note that the constraint system above actually has insufficient
  information to determine the type ``T0`` without guesswork. The
  `Overloading`_ section describes how the overload-selection
  mechanism is used to resolve this problem.

**Function application**
  A function application ``a(b)`` generates two
  constraints. First, the rvalue equality constraint ``T0 -> T1 ==R
  T(a)`` (for fresh type variables ``T0`` and ``T1``) captures the
  rvalue-to-lvalue conversion applied on the function (``a``) and
  decomposes the function type into its argument and result
  types. Second, the conversion constraint ``T(b) <c T0`` captures the
  requirement that the actual argument type (``b``) be convertible to
  the argument type of the function. Finally, the expression is given
  the type ``T1``, i.e.,  the result type of the function.

**Coercion/construction**
  A type coercion ``A(b)``, where ``A`` refers to a type, generates a
  construction constraint ``T(b) <C  A``, which requires that ``T(b)``
  either be a subtype of ``A`` or  that ``A`` have a constructor that
  accepts ``b``. The type of the expression is ``A``.

  Note that coercion/construction and function application use the
  syntax. Here, the constraint generator performs a shallow analysis
  of the type of the "function" argument (``A`` or ``a``, in the
  exposition above); if it obviously has metatype type, the expression
  is considered a coercion/construction rather than a function
  application. This particular area of the language needs more work.

**Subscripting**
  A subscript operation ``a[b]`` is similar to function
  application. A value member constraint ``T(a).__subscript == T0 -> T1``
  treats the subscript as a function from the key type to the
  value type, represented by fresh type variables ``T0`` and ``T1``,
  respectively. The constraint ``T(b) <c T0`` requires the key
  argument to be convertible to the key type, and the type of the
  subscript operation is ``T1``.

**Literals**
  A literal expression, such as ``17``, ``1.5``, or ``"Hello,
  world!``, is assigned a fresh type variable ``T0``. Additionally, a
  literal constraint is placed on that type variable depending on the
  kind of literal, e.g., "``T0`` is an integer literal."

**Function expressions**
  A function expression ``func (params) -> result { body }`` is
  assigned a function type based on the specified parameters and
  result type. The parameter pattern is walked to construct the
  parameter type of the function: wherever types are omitted for a
  parameter, a fresh type variable is introduced for that
  parameter. If the result type is not specified, a fresh type
  variable is introduced for the return type. The function expression
  is assigned a function type comprised of the computed parameter and
  return types. Note that the function body is not considered as part
  of the constraint system; it will be type-checked once the function
  expression has been given a concrete type.

**Explicit closures**
  An explicit closure ``{ body }`` is assigned a function type based
  on the (implied) parameters and the type of the body. Each
  positional parameter (``$1``, ``$2``, ..., ``$N``) is assigned a fresh type
  variable (call them ``T1``, ``T2``, ..., ``TN``). Then, constraint
  generation considers the body. The explicit closure is is given the
  type ``(T1, T2, ..., TN) -> T(body)``. 

  Note that, in contrast to the function expressions, the body of an
  explicit closure is considered as part of the same type-checking
  problem as the explicit closure itself. This decision corresponds to
  the higher-level decision to only perform type inference at the
  expression level, because the body of an explicit closure is always
  a single expression, while the body of a function expression is a
  compound statement.

**Object allocation**
  An object allocation ``new A(b)`` or ``new A[c]`` is assigned the
  type ``A`` or ``A[]``, respectively. For the single allocation case,
  the construction constraint ``T(b) <C A`` requires an ``A``
  constructor that accepts ``b``. For the multiple allocation case,
  the type checker (separately) checks that ``T(c)`` is an array bound
  type.

**Address of**
  An address-of expression ``&a`` always returns a ``[byref]``
  type. Therefore, it is assigned the type ``[byref] T0`` for a fresh
  type variable ``T0``. The subtyping constraint ``[byref] T0 <
  T(a)`` captures the requirement that input expression be an lvalue
  of some type.

Overloading
''''''''''''''''''''''''''

Overloading is the process of giving multiple, different definitions
to the same name. For example, we might overload a ``negate`` function
to work on both ``Int`` and ``Double`` types, e.g.::

  func negate(x : Int) -> Int { return -x }
  func negate(x : Double) -> Double { return -x }

Given that there are two definitions of ``negate``, what is the type of
the declaration reference expression ``negate``? If one selects the
first overload, the type is ``(Int) -> Int``; for the second overload,
the type is ``(Double) -> Double``. However, constraint generation
needs to assign some specific type to the expression, so that its
parent expressions can refer to that type.

Overloading in the type checker is modeled by introducing a fresh type
variable (call it ``T0``) for the type of the reference to an
overloaded declaration. Then, a disjunction constraint is introduced,
in which each term binds that type variable (via an exact equality
constraint) to the type produced by one of the overloads in the
overload set. In our negate example, the disjunction is 
``T0 := (Int) -> Int or T0 := (Double) -> Double``. The constraint
solver, discussed in the later section on `Constraint Solving`_,
explores both possible bindings, and the overloaded reference resolves
to whichever binding results in a solution that satisfies all
constraints [#]_.

Overloading can be introduced both by expressions that refer to sets
of overloaded declarations and by member constraints that end up
resolving to a set of overloaded declarations. One particularly
interesting case is the unresolved member reference, e.g.,
``.name``. As noted in the prior section, this generates the
constraint ``T0.name == T1``, where ``T0`` is a fresh type variable
that will be bound to the oneof type and ``T1`` is a fresh type
variable that will be bound to the type of the selected member. The
issue noted in the prior section is that this constraint does not give
the solver enough information to determine ``T0`` without
guesswork. However, we note that the type of a oneof member actually
has a regular structure. For example, consider the ``Optional`` type::

  oneof Optional<T> {
    none,
    value(value : T)
  }

The type of ``Optional<T>.none`` is ``Optional<T>``, while the type of
``Optional<T>.value`` is ``(value : T) -> Optional<T>``. In fact, the
type of a oneof element can have one of two forms: it can be ``T0``,
for a oneof element that has no extra data, or it can be ``T2 -> T0``,
where ``T2`` is the data associated with the oneof element.  Letting
``T2`` by a fresh type variable, we introduce a disjunction constraint
``T1 := T0 or T1 := T2 -> T0``. Note that, in both cases, the context
of the unresolved member reference helps determine which form matches,
and can therefore determine a binding for ``T0``. That binding allows
the resolution of the actual member.

Polymorphic Types
''''''''''''''''''''''''''''''''''''''''''''''

The Swift language includes "generics", a system of constrained
parameter polymorphism that enables polymorphic types and
functions. For example, one can implement a ``min`` function as,
e.g.,::

  func min<T : Comparable>(x : T, y : T) -> T {
    if y < x { return y }
    return x
  }

Here, ``T`` is effectively a type variable that can be replaced with
any concrete type, so long as that type conforms to the protocol
``Comparable``. The type of ``min`` is (internally) written as ``<T : Comparable> (x :
T, y : T) -> T``, which can be read as "for all ``T``, where ``T``
conforms to ``Comparable``, the type of the function is ``(x : T, y : T)
-> T``. Different uses of the ``min`` function may have different
bindings for the type variable ``T``.

When the constraint generator encounters a reference to a generic
function, it immediately replaces each of the type variables within
the function type with a fresh type variable, introduces constraints
on that type variable to match the constraints listed in the generic
function, and produces a monomorphic function type based on the
newly-generated type variables. For example, the first occurrence of
the declaration reference expression ``min`` would result in a type
``(x : T0, y : T0) -> T0``, where ``T0`` is a fresh type variable, as
well as the subtype constraint ``T0 < Comparable``, which expresses
protocol conformance. The next occurrence of the declaration reference
expression ``min`` would produce the type ``(x : T1, y : T1) -> T1``,
where ``T1`` is a fresh type variable (and therefore distinct from
``T0``), and so on. This replacement process is referred to as
"opening" the generic function type, and is a fairly simple (but
effective) way to model the use of polymorphic functions within the
constraint system without complicating the solver. Note that this
immediate opening of generic function types is only valid because
Swift does not support first-class polymorphic functions, e.g., one
cannot declare a variable of type ``<T> (T) -> T``.

Uses of generic types are also immediately opened by the constraint
solver. For example, consider the following generic dictionary type::

  class Dictionary<Key : Hashable, Value> {
    // ...
  }

When the constraint solver encounters the expression ``new
Dictionary``, it opens up the type ``Dictionary``---which has not been
provided with any specific generic arguments---to the type
``Dictionary<T0, T1>``, for fresh type variables ``T0`` and ``T1``,
and introduces the constraint ``T0 < Hashable``. This allows the
actual key and value types of the dictionary to be determined by the
context of the expression. As noted above for first-class polymorphic
functions, this immediate opening is valid because an unbound generic
type, i.e., one that does not have specified generic arguments, cannot
be used except where the generic arguments can be inferred.

Constraint Solving
-----------------------------
The primary purpose of the constraint solver is to take a given set of
constraints and determine the most specific type binding for each of the type
variables in the constraint system. As part of this determination, the
constraint solver also resolves overloaded declaration references by
selecting one of the overloads.

Solving the constraint systems generated by the Swift language can, in
the worst case, require exponential time. Even the class
Hindley-Milner type inference algorithm requires exponential type, and
the Swift type system introduces additional complications, especially
overload resolution. However, the problem size for any particular
expression is still fairly small, and the constraint solver can employ
a number of tricks to improve performance. The Performance_ section
describes some tricks that have been implemented or are planned, and
it is expected that the solver will be extended with additional tricks
going forward.

This section will focus on the basic ideas behind the design of the
solver, as well as the type rules that it applies.

Simplification
```````````````````
The constraint generation process introduces a number of constraints
that can be immediately solved, either directly (because the solution
is obvious and trivial) or by breaking the constraint down into a
number of smaller constraints. This process, referred to as
*simplification*, canonicalizes a constraint system for later stages
of constraint solving. It is also re-invoked each time the constraint
solver makes a guess (at resolving an overload or binding a type
variable, for example), because each such guess often leads to other
simplifications. When all type variables and overloads have been
resolved, simplification terminates the constraint solving process
either by detecting a trivial constraint that is not satisfied (hence,
this is not a proper solution) or by reducing the set of constraints
down to only simple constraints that are trivially satisfied.

The simplification process breaks down constraints into simpler
constraints, and each different kind of constraint is handled by
different rules based on the Swift type system. The constraints fall
into four categories: relational constraints, member constraints,
literal constraints, and archetype constraints. Only the first two
kinds of constraints have interesting simplification rules, and are
discussed in the following sections.

Relational Constraints
''''''''''''''''''''''''''''''''''''''''''''''''

Relational constraints describe a relationship between two types. This
category covers the equality, subtyping, conversion, and construction
constraints, and provides the most common simplifications. The
simplification of relationship constraints proceeds by comparing the
structure of the two types and applying the typing rules of the Swift
language to generate additional constraints. For example, if the
constraint is a conversion constraint::

  A -> B <c C -> D

then both types are function types, and we can break down this
constraint into two smaller constraints ``C < A`` and ``B < D`` by
applying the conversion rule for function types. Similarly, one can
destroy all of the various type constructors---tuple types, generic
type specializations, lvalue types, etc.---to produce simpler
requirements, based on the type rules of the language [#]_.

Relational constraints involving a type variable on one or both sides
generally cannot be solved directly. Rather, these constraints inform
the solving process later by providing possible type bindings,
described in the `Type Variable Bindings`_ section. The exception is
an equality constraint between two type variables, e.g., ``T0 ==
T1``. These constraints are simplified by unifying the equivalence
classes of ``T0`` and ``T1`` (using a basic union-find algorithm),
such that the solver need only determine a binding for one of the type
variables (and the other gets the same binding).

Member Constraints
'''''''''''''''''''''''''''''''''''''''''''

Member constraints specify that a certain type has a member of a given
name and provide a binding for the type of that member. A member
constraint ``A.member == B`` can be simplified when the type of ``A``
is determined to be a nominal or tuple type, in which case name lookup
can resolve the member name to an actual declaration. That declaration
has some type ``C``, so the member constraint is simplified to the
exact equality constraint``B := C``.

The member name may refer to a set of overloaded declarations. In this
case, the type ``C`` is a fresh type variable (call it ``T0``). A
disjunction constraint is introduced, each term of which new overload
set binds a different declaration's type to ``T0``, as described in
the section on Overloading_.

The kind of member constraint---type or value---also affects the
declaration type ``C``. A type constraint can only refer to member
types, and ``C`` will be the declared type of the named member. A
value constraint, on the other hand, can refer to either a type or a
value, and ``C`` is the type of a reference to that entity. For a
reference to a type, ``C`` will be a metatype of the declared type.

Derived Systems
````````````````````
The constraint generation process produces a single constraint system
consisting of a number of constraints. The simplification process then
breaks these constraints down into simpler constraints that 

Strategies
```````````````````````````````
The basic approach to constraint solving is to simplify the
constraints until they can no longer be simplified, then produce (and
check) educated guesses about which declaration from an overload set
should be selected and what concrete type should be bound to a given
type variable. Each guess is tested as an assumption, possibly with
other guesses, until the solver either arrives at a solution or
concludes that the guess was incorrect.

Within the implementation, each guess is modeled as a "derived"
constraint system. A derived constraint system inherits all of the
constraints, overload selections, and type variable bindings of the
parent constraint system from which it was derived, then adds one more
guess. As such, the solution space explored by the solver can be
viewed as a tree, where the top-most constraint system, which has no
parent, is the constraint system generated directly from the
expression. The leaves of the tree are either solutions to the
type-checking problem (where all constraints have been simplified
away) or represent sets of assumptions that do not lead to a
solution.

The following sections describe the techniques used by the solver to
produce derived constraint systems that explore the solution space.

Overload Selection
'''''''''''''''''''''''''''''''''''''''''''''''''''''
Overload selection is the simplest way to create derived constraint
systems. For an overload set that introduced a disjunction constraint 
``T0 := A1 or T0 := A2 or ... or T0 := AN`` into the constraint
system, one derived constraint system is created for each term in the
disjunction, corresponding to the selected overload. Each derived
constraint system then binds the type variable ``T0`` and explores
whether the selected overload leads to a suitable solution.

Type Variable Bindings
'''''''''''''''''''''''''''''''''''''''''''''''''''''
A second way in which the solver creates derived constraint systems is
to guess at the concrete type to which a given type variable should be
bound. That type binding is then introduced in a new, derived
constraint system to determine if the binding is feasible.

The solver does not conjure concrete type bindings from nothing, nor
does it perform an exhaustive search. Rather, it uses the constraints
placed on that type variable to produce potential candidate
types. There are several strategies employed by the solver.

Meets and Joins
..........................................
A given type variable ``T0`` often has relational constraints
placed on it that relate it to concrete types, e.g., ``T0 <c Int`` or
``Float <c T0``. In these cases, we can use the concrete types as a
starting point to make educated guesses for the type ``T0``.

To determine an appropriate guess, the relational constraints placed
on the type variable are categorized. Given a relational constraint of the form 
``T0 <? A`` (where ``<?`` is one of ``<``, ``<t``, or ``<c``), where
``A`` is some concrete type, ``A`` is said to be  "above"
``T0``. Similarly, given a constraint of the form ``B <? T0`` for a
concrete type ``B``, ``B`` is said to be "below" ``T0``. The
above/below terminologies comes from a visualization of the lattice of
types formed by the conversion relationship, e.g., there is an edge
``A -> B`` in the latter if ``A`` is convertible to ``B``. ``B`` would
therefore be higher in the lattice than ``A``, and the topmost element
of the lattice is the element to which all types can be converted,
``protocol<>`` (often called "top"). 

The concrete types "above" and "below" a given type variable provide
bounds on the possible concrete types that can be assigned to that
type variable. The solver computes [#]_ the join of the types "below"
the type variable, i.e., the most specific (lowest) type to which all
of the types "below" can be converted, and uses that join as a
starting guess.


Supertype Fallback
..........................................
The join of the "below" types computed as a starting point may be too
specific, due to constraints that involve the type variable but
weren't simple enough to consider as part of the join. To cope with
such cases, if no solution can be found with the join of the "below"
types, the solver creates a new set of derived constraint systems with
weaker assumptions, corresponding to each of the types that the join
is directly convertible to. For example, if the join was some class
``Derived``, the supertype fallback would then try the class ``Base``
from which ``Derived`` directly inherits. This fallback process
continues until the types produced are no longer convertible to the
meet of types "above" the type variable, i.e., the least specific
(highest) type from which all of the types "above" the type variable
can be converted [#]_.


Default Literal Types
..........................................
If a type variable is bound by a literal constraint, e.g., "``T0``  is
an integer literal", then the constraint solver will guess that the
type variable can be bound to the default literal type. For example,
``T0`` would get the default integer literal type ``Int``, allowing
one to type-check expressions with too little type information to
determine the types of these literals, e.g., ``-1``.

Comparing Solutions
`````````````````````````
The solver explores a potentially large solution space, and it is
possible that it will find multiple solutions to the constraint system
as given. Such cases are not necessarily ambiguities, because the
solver can then compare the solutions to to determine whether one of
the solutions is better than all of the others. To do so, it compares
the concrete type variable bindings and selected overloads from each
pair of solutions:

- If two type variables have different concrete type bindings in the
  two solutions, the two type variables are compared. If the concrete
  type bound in the first solution is convertible to the concrete type
  bound in the second solution, then the binding in the first solution
  is more specific than the binding in the second solution.

  If neither concrete type is convertible to the other, and there is a
  literal constraint on the type variable, then the binding in the
  first solution is more specific if the bound type is the same as the
  default literal type for that literal constraint (and the binding in
  the second solution is not). This rule therefore prefers the use of
  the default literal types over other types, and ends up breaking
  ambiguities in cases where there are several literals.

- If two overload sets have different selected overloads in the two
  soluions, the overloads are compared [#]_. If the type of the
  overload picked in the first solution is convertible to the type of
  the overload picked in the second solution, then the overload
  selection in the first solution is more specific than the binding in
  the second solution.

If any type variable bindings or overload selections in one solution
are more specific than their corresponding binding or select in
another solution, and no type variable binding or overload selection
is less specific, then the first solution is more specific. The best
solution is the solution that is more specific than all other
solutions.

Solution Application
-------------------------
Once the solver has produced a solution to the constraint system, that
solution must be applied to the original expression to produce a fully
type-checked expression that makes all implicit conversions and
resolved overloads explicit. This application process walks the
expression tree from the leaves to the root, rewriting each expression
node based on the kind of expression:

*Declaration references*
  Declaration references are rewritten with the precise type of the
  declaraion as referenced. For overloaded declaration references, the
  ``Overload*Expr`` node is replaced with a simple declaration
  reference expression. For references to polymorphic functions or
  members of generic types, a ``SpecializeExpr`` node is introduced to
  provide substitutions for all of the generic parameters.

*Member references*
  References to members are similar to declaration
  references. However, they have the added constraint that the base
  expression needs to be a reference. Therefore, an rvalue of
  non-reference type will be materialized to produce the necessary
  reference.

*Literals*
  Literals are converted to the appropriate literal type, which
  typically involves introducing calls to ``convertFromXXXLiteral``
  functions.

*Function expressions*
  Since the function expression has acquired a complete function type,
  the body of the function expression is type-checked with that
  complete function type. (This operation can be delayed).

The solution application step cannot fail, because every potential
failure is modeled as a constraint in the constraint system. If any
failures do occur at this step, it is a bug in the type checker.

Performance
-----------------
The performance of the type checker is currently terrible. We plan to
implement a number of heuristics to prune the solution space more
quickly, since much of the work we perform is completely wasted.

Diagnostics
-----------------
The diagnostics produced by the type checker are currently
terrible. We plan to do something about this, eventually. We also
believe that we can implement some heroics, such as spell-checking
that takes into account the surrounding expression to only provide
well-typed suggestions.

.. [#] It is possible that both overloads will result in a solution,
   in which case the solutions will be ranked based on the rules
   discussed in the section `Comparing Solutions`_.

.. [#] As of the time of this writing, the type rules of Swift have
  not specifically been documented outside of the source code. The
  constraints-based type checker contains a function ``matchTypes``
  that documents and implements each of these rules. A future revision
  of this document will provide a more readily-accessible version.

.. [#] More accurately, as of this writing, "will compute". The solver
  doesn't current compute meets and joins properly. Rather, it
  arbitrarily picks one of the constraints "below" to start with.

.. [#] Again, as of this writing, the solver doesn't actually compute
  meets and joins, so the solver continues until it runs out of
  supertypes to enumerate.

.. [#] This overload resolution has yet to be implemented. Moreover,
   there is an optimization opportunity here to explore the
   more-specific overloads before the less-specific overloads.

