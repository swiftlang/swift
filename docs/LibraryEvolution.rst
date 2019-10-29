:orphan:

.. default-role:: term
.. title:: Library Evolution Support in Swift ("Resilience")

:Author: Jordan Rose
:Author: John McCall

.. note::

    This document uses some Sphinx-specific features which are not available on
    GitHub. For proper rendering, download and build the docs yourself. Jordan
    Rose also posts occasional snapshots at
    https://jrose-apple.github.io/swift-library-evolution/.

One of Swift's primary design goals is to allow efficient execution of code
without sacrificing load-time abstraction of implementation.

Abstraction of implementation means that code correctly written against a
published interface will correctly function when the underlying implementation
changes to anything which still satisfies the original interface. There are
many potential reasons to provide this sort of abstraction. Apple's primary
interest is in making it easy and painless for our internal and external
developers to improve the ecosystem of Apple products by creating good and
secure programs and libraries; subtle deployment problems and/or unnecessary
dependencies on the behavior of our implementations would work against these
goals.

Our current design in Swift is to provide opt-out load-time abstraction of
implementation for all language features. Alone, this would either incur
unacceptable cost or force widespread opting-out of abstraction. We intend to
mitigate this primarily by designing the language and its implementation to
minimize unnecessary and unintended abstraction:

* Avoiding unnecessary language guarantees and taking advantage of that
  flexibility to limit load-time costs.

* Within the domain that defines an entity, all the details of its
  implementation are available.

* When entities are not exposed outside their defining module, their
  implementation is not constrained.

* By default, entities are not exposed outside their defining modules. This is
  independently desirable to reduce accidental API surface area, but happens to
  also interact well with the performance design.

This last point is a specific case of a general tenet of Swift: **the default
behavior is safe**. Where possible, choices made when an entity is first
published should not limit its evolution in the future.

.. contents:: :local:


Introduction
============

This model is intended to serve library designers whose libraries will evolve
over time. Such libraries must be both `backwards-compatible`, meaning that
existing clients should continue to work even when the library is updated, and
`forwards-compatible`, meaning that future clients will be able run using the
current version of the library. In simple terms:

- Last year's apps should work with this year's library.
- Next year's apps should work with this year's library.

This document will frequently refer to a *library* which vends public APIs, and
a single *client* that uses them. The same principles apply even when multiple
libraries and multiple clients are involved.

This document is primarily concerned with `binary compatibility`, i.e. what
changes can safely be made to a library between releases that will not break
memory-safety or type-safety, or cause clients to fail to run at all. A
secondary concern is identifying `binary-compatible source-breaking changes
<binary-compatible source-breaking change>`, where clients compiled against the
previous version of a library are likely to behave differently than clients
compiled against the new version of the library.

.. note::

    These rules do not (and cannot) guarantee that a change is *semantically*
    backwards-compatible or forwards-compatible. *Any* change to a library's
    existing API that affects its observable behavior may affect clients. It is
    the responsibility of a library author to be sure that the changes they are
    making are *semantically* correct, preserving the preconditions,
    postconditions, and invariants of previously-published APIs.

This model is largely not of interest to libraries that are bundled with their
clients (distribution via source, static library, or embedded/sandboxed dynamic
library, as used by the `Swift Package Manager`_). Because a client always uses
a particular version of such a library, there is no need to worry about
backwards- or forwards-compatibility at the binary level. Just as developers
with a single app target are not forced to think about access control, anyone
writing a bundled library should (ideally) not be required to use any of the
annotations described below in order to achieve full performance.

.. _Swift Package Manager: https://swift.org/package-manager/

.. note::

    This model may, however, be useful for library authors that want to
    preserve *source* compatibility, and it is hoped that the tool for
    `Checking Binary Compatibility`_ described below will also be useful for
    this purpose. Additionally, we may decide to use some of these annotations
    as performance hints for *non-*\ optimized builds.

The term "resilience" comes from the occasional use of "fragile" to describe
certain constructs that have very strict binary compatibility rules. For
example, a client's use of a C struct is "fragile" in that if the library
changes the fields in the struct, the client's use will "break". In Swift,
changing the fields in a struct will not automatically cause problems for
existing clients, so we say the struct is "resilient".


Supported Evolution
===================

This section describes the various changes that are safe to make when releasing
a new version of a library, i.e. changes that will not break binary
compatibility. They are organized by declaration type.

Anything *not* listed in this document should be assumed unsafe.


Top-Level Functions
~~~~~~~~~~~~~~~~~~~

A versioned top-level function is fairly restricted in how it can be changed.
The following changes are permitted:

- Changing the body of the function.
- Changing *internal* parameter names (i.e. the names used within the function
  body, not the labels that are part of the function's full name).
- Reordering generic requirements (but not the generic parameters themselves).
- Adding a default argument expression to a parameter.
- Changing or removing a default argument is a `binary-compatible
  source-breaking change`.
- The ``@discardableResult`` and ``@warn_unqualified_access`` attributes may
  be added to a function without any additional versioning information.

No other changes are permitted; the following are particularly of note:

- A versioned function may not change its parameters or return type.
- A versioned function may not change its generic requirements.
- A versioned function may not change its external parameter names (labels).
- A versioned function may not add, remove, or reorder parameters, whether or
  not they have default arguments.
- A versioned function that throws may not become non-throwing or vice versa.
- The ``@escaping`` attribute may not be added to or removed from a parameter.
  It is not a `versioned attribute` and so there is no way to guarantee that it
  is safe when a client deploys against older versions of the library.


Inlinable Functions
-------------------

Functions are a very common example of resilience: the function's declaration
is published as API, but its body may change between library versions as long
as it upholds the same semantic contracts. This applies to other function-like
constructs as well: initializers, accessors, and deinitializers.

However, sometimes it is useful to provide the body to clients as well. There
are a few common reasons for this:

- The function only performs simple operations, and so inlining it will both
  save the overhead of a cross-library function call and allow further
  optimization of callers.

- The function accesses a frozen struct with non-public members; this
  allows the library author to preserve invariants while still allowing
  efficient access to the struct.

- The function is generic and its performance may be greatly increased by
  specialization in the client.

A versioned function marked with the ``@inlinable`` attribute makes its body
available to clients as part of the module's public interface. ``@inlinable``
is a `versioned attribute`; clients may not assume that the body of the
function is suitable when deploying against older versions of the library.

Clients are not required to inline a function marked ``@inlinable``.

.. note::

    It is legal to change the implementation of an inlinable function in the
    next release of the library. However, any such change must be made with the
    understanding that it will not affect existing clients. This is the
    standard example of a `binary-compatible source-breaking change`.

Any local functions or closures within an inlinable function are treated as
``@_alwaysEmitIntoClient`` (see below). A client that inlines the containing
function must emit its own copy of the local functions or closures. This is
important in case it is necessary to change the inlinable function later;
existing clients should not be depending on internal details of the previous
implementation.

Removing the ``@inlinable`` attribute completely---say, to reference private
implementation details that should not be `versioned <versioned entity>`---is a
safe change. However, existing clients will of course not be affected by this
change, and any future use of the function must take this into account.

Although they are not a supported feature for arbitrary libraries at this time,
`transparent`_ functions are implicitly marked ``@inlinable``.

.. _transparent: https://github.com/apple/swift/blob/master/docs/TransparentAttr.rst


Restrictions on Inlinable Functions
-----------------------------------

Because the body of an inlinable function (or method, accessor, initializer,
or deinitializer) will be inlined into another module, it must not make any
assumptions that rely on knowledge of the current module. Here is a trivial
example using methods::

    public struct Point2D {
      var x, y: Double
      public init(x: Double, y: Double) { /*...*/ }
    }

    extension Point2D {
      @inlinable public func distance(to other: Point2D) -> Double {
        let deltaX = self.x - other.x
        let deltaY = self.y - other.y
        return sqrt(deltaX*deltaX + deltaY*deltaY)
      }
    }

As written, this ``distance`` method is not safe to inline. The next release
of the library could very well replace the implementation of ``Point2D`` with a
polar representation::

    public struct Point2D {
      var r, theta: Double
      public init(x: Double, y: Double) { /*...*/ }
    }

and the ``x`` and ``y`` properties have now disappeared. To avoid this, the
bodies of inlinable functions have the following restrictions:

- They may not define any local types.

- They must not reference any ``private`` or ``fileprivate`` entities.

- They must not reference any ``internal`` entities except for those that have
  been ``versioned <versioned entity>` and those declared ``@inlinable``. See
  below for a discussion of versioning internal API.

- They must not reference any entities from the current module introduced
  after the function was made inlinable, except under appropriate availability
  guards.


Always Emit Into Client
-----------------------

A function, computed property or subscript annotated as ``@_alwaysEmitIntoClient``
is similar to an ``@inlinable`` declaration, except the declaration is
not part of the module's ABI, meaning that the client must always emit
their own copy.

As a result, removing a declaration annotated as ``@_alwaysEmitIntoClient``
is a binary-compatible source-breaking change.

.. admonition:: TODO

    The implementation of ``@_alwaysEmitIntoClient`` is incomplete and
    should probably graduate to having its own evolution proposal.

Default Argument Expressions
----------------------------

Default argument expressions for functions that are public, versioned, or
inlinable are implicitly ``@_alwaysEmitIntoClient``. They are subject to
similar restrictions:

- They may not define any local types.

- They must not reference any non-``public`` entities.

- They must not reference any entities from the current module introduced
  after the default argument was added, except under appropriate availability
  guards.

A default argument implicitly has the same availability as the function it is
attached to. Because default argument expressions can be added and removed, a
client that uses one must always emit its own copy of the implementation.


Top-Level Variables and Constants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a versioned module-scope variable declared with ``var``, the following
changes are permitted:

- Adding (but not removing) a public setter to a computed variable.
- Adding or removing a non-public, non-versioned setter.
- Changing from a stored variable to a computed variable, or vice versa, as
  long as a previously versioned setter is not removed.
- As a special case of the above, adding or removing ``lazy`` from a stored
  property.
- Changing the body of an accessor.
- Adding or removing an observing accessor (``willSet`` or ``didSet``) to/from
  an existing variable. This is effectively the same as modifying the body of a
  setter.
- Changing the initial value of a stored variable.
- Adding or removing ``weak`` from a variable with ``Optional`` type.
- Adding or removing ``unowned`` from a variable.
- Adding or removing ``@NSCopying`` to/from a variable.

If a public setter is added after the property is first exposed (whether the
property is stored or computed), it must be versioned independently of the
property itself.

.. admonition:: TODO

    This needs syntax.

Additionally, for a module-scope constant declared with ``let``, the following
changes are permitted:

- Changing the value of the constant.

It is *not* safe to change a ``let`` constant into a variable or vice versa.
Top-level constants are assumed not to change for the entire lifetime of the
program once they have been initialized.

.. admonition:: TODO

    We could make it safe to turn a read-only ``var`` into a ``let``, but do we
    want to? We would have to come up with syntax for declaring when it
    changed, at least.


Giving Up Flexibility
---------------------

Both top-level constants and variables can be marked ``@inlinableAccess`` to
allow clients to access them more efficiently. This restricts changes a fair
amount:

- Adding a versioned setter to a computed variable is still permitted.
- Adding or removing a non-public, non-versioned setter is still permitted.
- Changing from stored to computed or vice versa is forbidden, because it would
  break existing clients.
- Similarly, adding or removing ``lazy`` is forbidden.
- Changing the body of an accessor is a `binary-compatible source-breaking
  change`.
- Adding/removing observing accessors is likewise a `binary-compatible
  source-breaking change`.
- Changing the initial value of a stored variable is still permitted.
- Changing the value of a constant is a `binary-compatible source-breaking
  change`.
- Adding or removing ``weak`` is forbidden.
- Adding or removing ``unowned`` is forbidden.
- Adding or removing ``@NSCopying`` to/from a variable is `binary-compatible
  source-breaking change`.

.. admonition:: TODO

    It Would Be Nice(tm) to allow marking the *getter* of a top-level variable
    inlinable while still allowing the setter to change. This would need
    syntax, though.

Any inlinable accessors must follow the rules for `inlinable functions`_, as
described above.

Note that if a constant's initial value expression has any observable side
effects, including the allocation of class instances, it must not be treated
as inlinable. A constant must always behave as if it is initialized exactly
once.

.. admonition:: TODO

    Is this a condition we can detect at compile-time? Do we have to be
    restricted to things that can be lowered to compile-time constants?

.. admonition:: TODO

    ``@inlinableAccess`` isn't implemented yet, but for computed properties we
    already allow putting ``@inlinable`` on the accessors individually. That
    doesn't support all the use cases, like promising that a stored property
    will remain stored, but it also provides flexibility in only making *one*
    accessor inlinable. Is that important?

Structs
~~~~~~~

Swift structs are a little more flexible than their C counterparts. By default,
the following changes are permitted:

- Reordering any existing members, including stored properties.
- Adding any new members, including stored properties.
- Changing existing properties from stored to computed or vice versa.
- As a special case of the above, adding or removing ``lazy`` from a stored
  property.
- Changing the body of any methods, initializers, or accessors.
- Adding or removing an observing accessor (``willSet`` or ``didSet``) to/from
  an existing property. This is effectively the same as modifying the body of a
  setter.
- Removing any non-public, non-versioned members, including stored properties.
- Adding a new protocol conformance (with proper availability annotations).
- Removing conformances to non-public protocols.

The important most aspect of a Swift struct is its value semantics, not its
layout.

It is not safe to add or remove ``mutating`` or ``nonmutating`` from a member
or accessor within a struct. These modifiers are not `versioned attributes
<versioned attribute>` and as such there is no safety guarantee for a client
deploying against an earlier version of the library.


Methods and Initializers
------------------------

For the most part struct methods and initializers are treated exactly like
top-level functions. They permit all of the same modifications and can also be
marked ``@inlinable``, with the same restrictions. Inlinable initializers must
always delegate to another initializer or assign an entire value to ``self``,
since new properties may be added between new releases. For the same reason,
initializers declared outside of the struct's module must always delegate to
another initializer or assign to ``self``.


Properties
----------

Struct properties behave largely the same as top-level bindings. They permit
all of the same modifications, and also allow adding or removing an initial
value entirely.

Struct properties can also be marked ``@inlinableAccess``, with the same
restrictions as for top-level bindings. An inlinable stored property may not
become computed, but the offset of its storage within the struct is not
necessarily fixed.

Like top-level constants, it is *not* safe to change a ``let`` property into a
variable or vice versa. Properties declared with ``let`` are assumed not to
change for the entire lifetime of the program once they have been initialized.


Subscripts
----------

Subscripts behave largely the same as properties, except that there are no
stored subscripts. This means that the following changes are permitted:

- Adding (but not removing) a public setter.
- Adding or removing a non-public, non-versioned setter.
- Changing the body of an accessor.
- Changing index parameter internal names (i.e. the names used within the
  accessor bodies, not the labels that are part of the subscript's full name).
- Reordering generic requirements (but not the generic parameters themselves).
- Adding a default argument expression to an index parameter.
- Changing or removing a default argument is a `binary-compatible
  source-breaking change`.

Like properties, subscripts can be marked ``@inlinableAccess``, which makes
changing the body of an accessor a `binary-compatible source-breaking change`.
Any inlinable accessors must follow the rules for `inlinable functions`_, as
described above.


New Conformances
----------------

If a conformance is added to a type in version 1.1 of a library, it's important
that it isn't accessed in version 1.0. This is implied if the protocol itself
was introduced in version 1.1, but needs special handling if both the protocol
and the type were available earlier. In this case, the conformance *itself*
needs to be labeled as being introduced in version 1.1, so that the compiler
can enforce its safe use.

.. note::

    This may feel like a regression from Objective-C, where `duck typing` would
    allow a ``Wand`` to be passed as an ``id <MagicType>`` without ill effects.
    However, ``Wand`` would still fail a ``-conformsToProtocol:`` check in
    version 1.0 of the library, and so whether or not the client code will work
    is dependent on what should be implementation details of the library.

We've considered two possible syntaxes for this::

    @available(1.1)
    extension Wand : MagicType {/*...*/}

and

::

    extension Wand : @available(1.1) MagicType {/*...*/}

The former requires fewer changes to the language grammar, but the latter could
also be used on the declaration of the type itself (i.e. the ``struct``
declaration).

If we went with the former syntax, applying ``@available`` to an extension
would override the default availability of entities declared within the
extension; unlike access control, entities within the extension may freely
declare themselves to be either more or less available than what the extension
provides.

We could also implement a ``@_alwaysEmitIntoClient``  attribute for conformances.
This introduces its own challenges with runtime uniquing of witness tables now
necessary for conformances.


Frozen Structs
--------------

To opt out of this flexibility, a struct may be marked ``@frozen``.
This promises that no stored properties will be added to or removed from the
struct, even non-public ones. Additionally, all versioned instance stored
properties in a ``@frozen`` struct are implicitly declared
``@inlinable`` (as described above for top-level variables). In effect:

- Reordering stored instance properties (public or non-public) is not permitted.
  Reordering all other members is still permitted.
- Adding new stored instance properties (public or non-public) is not permitted.
  Adding any other new members is still permitted.
- Changing existing instance properties from stored to computed or
  vice versa is not permitted.
- Similarly, adding or removing ``lazy`` from a stored property is not
  permitted.
- Changing the body of any *existing* methods, initializers, computed property
  accessors, or non-instance stored property accessors is permitted. Changing
  the body of a stored instance property observing accessor is permitted if the
  property is not `versioned <versioned entity>`, and considered a
  `binary-compatible source-breaking change` if it is.
- Adding or removing observing accessors from any
  `versioned <versioned entity>` stored instance properties (public or
  non-public) is not permitted.
- Removing stored instance properties is not permitted. Removing any other
  non-public, non-versioned members is still permitted.
- Adding a new protocol conformance is still permitted.
- Removing conformances to non-public protocols is still permitted.

Additionally, if the type of any stored instance property includes a struct or
enum, that struct or enum must be `versioned <versioned entity>`. This includes
generic parameters and members of tuples.

.. note::

    The above restrictions do not apply to ``static`` properties of
    ``@frozen`` structs. Static members effectively behave as top-level
    functions and variables.

While adding or removing stored properties is forbidden, existing properties may
still be modified in limited ways:

- An existing non-public, non-versioned property may change its access level to
  any other non-public access level.
- A non-versioned ``internal`` property may be versioned (see `Versioning
  Internal Declarations`_).
- A versioned ``internal`` property may be made ``public`` (without changing
  its version).

An initializer of a frozen struct may be declared ``@inlinable`` even
if it does not delegate to another initializer, as long as the ``@inlinable``
attribute, or the initializer itself, is not introduced earlier than the
``@frozen`` attribute and the struct has no non-versioned stored
properties.

A ``@frozen`` struct is *not* guaranteed to use the same layout as a C
struct with a similar "shape". If such a struct is necessary, it should be
defined in a C header and imported into Swift.

.. note::

    We can add a *different* feature to control layout some day, or something
    equivalent, but this feature should not restrict Swift from doing useful
    things like minimizing member padding. At the very least, Swift structs
    don't guarantee the same tail padding that C structs do.

.. note::

    Hypothetically, we could use a different model where a ``@frozen``
    struct only guarantees the "shape" of the struct, so to speak, while
    leaving all property accesses to go through function calls. This would
    allow stored properties to change their accessors, or (with the Behaviors
    proposal) to change a behavior's implementation, or change from one
    behavior to another. However, the *most common case* here is probably just
    a simple C-like struct that groups together simple values, with only public
    stored properties and no observing accessors, and having to opt into direct
    access to those properties seems unnecessarily burdensome. The struct is
    being declared ``@frozen`` for a reason, after all: it's been
    discovered that its use is causing performance issues.

    Consequently, as a first pass we may just require all stored properties in
    a ``@frozen`` struct, public or non-public, to have trivial
    accessors, i.e. no observing accessors and no behaviors.

``@frozen`` is a `versioned attribute`. This is so that clients can
deploy against older versions of the library, which may have a different layout
for the struct. (In this case the client must manipulate the struct as if the
``@frozen`` attribute were absent.)


Enums
~~~~~

By default, a library owner may add new cases to a public enum between releases
without breaking binary compatibility. As with structs, this results in a fair
amount of indirection when dealing with enum values, in order to potentially
accommodate new values. More specifically, the following changes are permitted:

- Adding a new case.
- Reordering existing cases is a `binary-compatible source-breaking change`. In
  particular, if an enum is RawRepresentable, changing the raw representations
  of cases may break existing clients who use them for serialization.
- Adding a raw type to an enum that does not have one.
- Removing a non-public, non-versioned case.
- Adding any other members.
- Removing any non-public, non-versioned members.
- Adding a new protocol conformance (with proper availability annotations).
- Removing conformances to non-public protocols.

.. note::

    If an enum value has a known case, or can be proven to belong to a set of
    known cases, the compiler is of course free to use a more efficient
    representation for the value, just as it may discard fields of structs that
    are provably never accessed.

.. note::

    Non-public cases in public enums don't exist at the moment, but they *can*
    be useful, and they require essentially the same implementation work as
    cases added in future versions of a library.

Adding or removing the ``@objc`` attribute from an enum is not permitted; this
affects the enum's memory representation and is not backwards-compatible.


Initializers
------------

For the most part enum initializers are treated exactly like top-level
functions. They permit all of the same modifications and can also be marked
``@inlinable``, with the same restrictions.


Methods and Subscripts
----------------------

The rules for enum methods and subscripts are identical to those for struct
members.


Frozen Enums
------------

A library owner may opt out of this flexibility by marking a versioned enum as
``@frozen``. A "frozen" enum may not have any cases with less access than the
enum itself, and may not add new cases in the future. This guarantees to
clients that the enum cases are exhaustive. In particular:

- Adding new cases is not permitted.
- Reordering existing cases is not permitted.
- Removing a non-public case is not applicable.
- Adding a raw type is still permitted.
- Adding any other members is still permitted.
- Removing any non-public, non-versioned members is still permitted.
- Adding a new protocol conformance is still permitted.
- Removing conformances to non-public protocols is still permitted.

.. note::

    Were a public "frozen" enum allowed to have non-public cases, clients of
    the library would still have to treat the enum as opaque and would still
    have to be able to handle unknown cases in their ``switch`` statements.

``@frozen`` is a `versioned attribute`. This is so that clients can deploy
against older versions of the library, which may have non-public cases in the
enum. (In this case the client must manipulate the enum as if the ``@frozen``
attribute were absent.) All cases that are not versioned become implicitly
versioned with this number.

Even for default "non-frozen" enums, adding new cases should not be done
lightly. Any clients attempting to do an exhaustive switch over all enum cases
will likely not handle new cases well.

.. note::

    One possibility would be a way to map new cases to older ones on older
    clients. This would only be useful for certain kinds of enums, though, and
    adds a lot of additional complexity, all of which would be tied up in
    versions. Our generalized switch patterns probably make it hard to nail
    down the behavior here.


Protocols
~~~~~~~~~

There are very few safe changes to make to protocols and their members:

- A default may be added to an associated type.
- A new optional requirement may be added to an ``@objc`` protocol.
- All members may be reordered, including associated types.
- Changing *internal* parameter names of function and subscript requirements
  is permitted.
- Reordering generic requirements is permitted (but not the generic parameters
  themselves).
- The ``@discardableResult`` and ``@warn_unqualified_access`` attributes may
  be added to a function requirement without any additional versioning
  information.

New requirements can be added to a protocol. However, restrictions around
existential types mean that adding new associated types or non-type requirements
involving ``Self`` can break source compatibility. For this reason, the following
are `binary-compatible source-breaking changes <binary-compatible source-breaking change>`:

- A new non-type requirement may be added to a protocol, as long as it has an
  unconstrained default implementation in a protocol extension of the
  protocol itself or some other protocol it refines.
- A new associated type requirement may be added as long as it has a
  default.

All other changes to the protocol itself are forbidden, including:

- Adding or removing refined protocols.
- Removing any existing requirements (type or non-type).
- Removing the default type of an associated type.
- Making an existing requirement optional.
- Making a non-``@objc`` protocol ``@objc`` or vice versa.
- Adding or removing protocols and superclasses from the inheritance
  clause of an associated type.
- Adding or removing constraints from the ``where`` clause of
  the protocol or an associated type.

Protocol extensions may be more freely modified; `see below`__.

__ #protocol-extensions

Classes
~~~~~~~

Because class instances are always accessed through references, they are very
flexible and can change in many ways between releases. Like structs, classes
support all of the following changes:

- Reordering any existing members, including stored properties.
- Changing existing properties from stored to computed or vice versa.
- As a special case of the above, adding or removing ``lazy`` from a stored
  property.
- Changing the body of any methods, initializers, or accessors.
- Adding or removing an observing accessor (``willSet`` or ``didSet``) to/from
  an existing property. This is effectively the same as modifying the body of a
  setter.
- Removing any non-public, non-versioned members, including stored properties.
- Adding a new protocol conformance (with proper availability annotations).
- Removing conformances to non-public protocols.

Omitted from this list is the free addition of new members. Here classes are a
little more restrictive than structs; they only allow the following changes:

- Adding a new convenience initializer.
- Adding a new designated initializer, if the class is not ``open``.
- Adding a deinitializer.
- Adding new, non-overriding method, subscript, or property.
- Adding a new overriding member, though if the class is ``open`` the type of
  the member may not deviate from the member it overrides. Changing the type
  could be incompatible with existing overrides in subclasses.

Finally, classes allow the following changes that do not apply to structs:

- A public class may be made ``open`` if it is not already marked ``final``.
- A non-``open`` public class may be marked ``final``.
- Removing an explicit deinitializer. (A class with no declared deinitializer
  effectively has an implicit deinitializer.)
- "Moving" a method, subscript, or property up to its superclass. The
  declaration of the original member must remain along with its original
  availability, but its body may consist of simply calling the new superclass
  implementation.
- A non-final override of a method, subscript, property, or initializer may be
  removed as long as the generic parameters, formal parameters, and return type
  *exactly* match the overridden declaration. Any existing callers should
  automatically use the superclass implementation.
- Within an ``open`` class, any public method, subscript, or property may be
  marked ``open`` if it is not already marked ``final``.
- Any method, subscript, or property may be marked ``final`` if it is not
  already marked ``open``.
- ``@IBOutlet``, ``@IBAction``, ``@IBInspectable``, and ``@GKInspectable`` may
  be added to a member without providing any extra version information.
  Removing any of these is a `binary-compatible source-breaking change` if the
  member remains ``@objc``, and disallowed if not.
- Likewise, ``@IBDesignable`` may be added to a class without providing any
  extra version information. Removing it is considered a `binary-compatible
  source-breaking change`.
- Changing a class's superclass ``A`` to another class ``B``, *if* class ``B``
  is a subclass of ``A`` *and* class ``B``, along with any superclasses between
  it and class ``A``, were introduced in the latest version of the library.

.. admonition:: TODO

    This last is very tricky to get right. We've seen it happen a few times in
    Apple's SDKs, but at least one of them, `NSCollectionViewItem`_ becoming a
    subclass of NSViewController instead of the root class NSObject, doesn't
    strictly follow the rules. While NSViewController was introduced in the
    same version of the OS, its superclass, NSResponder, was already present.
    If a client app was deploying to an earlier version of the OS, would
    NSCollectionViewItem be a subclass of NSResponder or not? How would the
    compiler be able to enforce this?

.. admonition:: TODO

    Both ``final`` and ``open`` may be applied to a declaration after it has
    been made public. However, these need to be treated as
    `versioned attributes <versioned attribute>`. It's not clear what syntax
    should be used for this.

.. _NSCollectionViewItem: https://developer.apple.com/library/mac/documentation/Cocoa/Reference/NSCollectionViewItem_Class/index.html

Other than those detailed above, no other changes to a class or its members
are permitted. In particular:

- An ``open`` class or member cannot become non-``open``.
- ``final`` may not be removed from a class or its members. (The presence of
  ``final`` enables optimization.)
- ``dynamic`` may not be added to *or* removed from any members. Existing
  clients would not know to invoke the member dynamically.
- A ``final`` override of a member may *not* be removed, even if the type
  matches exactly; existing clients may be performing a direct call to the
  implementation instead of using dynamic dispatch.
- ``@objc`` and ``@nonobjc`` may not be added to or removed from the class or
  any existing members.
- ``@NSManaged`` may not be added to or removed from any existing members.

.. admonition:: TODO

    ``@NSManaged`` as it is in Swift 4.2 exposes implementation details to
    clients in a bad way. If we want to use ``@NSManaged`` in frameworks with
    binary compatibility concerns, we need to fix this. rdar://problem/20829214


Initializers
------------

New designated initializers may not be added to an ``open`` class. This would
change the inheritance of convenience initializers, which existing subclasses
may depend on. An ``open`` class also may not change a convenience initializer
into a designated initializer or vice versa.

A new ``required`` initializer may be added to a class only if it is a
convenience initializer; that initializer may only call existing ``required``
initializers. An existing initializer may not be marked ``required``.

.. admonition:: TODO

    This implies a different rule for inheriting ``required`` convenience
    initializers than non-required convenience initializers, which is not
    currently implemented.

All of the modifications permitted for top-level functions are also permitted
for class initializers. Convenience initializers may be marked ``@inlinable``,
with the same restrictions as top-level functions; designated initializers may
not.


Methods
-------

Both class and instance methods allow all of the modifications permitted for
top-level functions, but the potential for overrides complicates things a
little. They allow the following changes:

- Changing the body of the method.
- Changing *internal* parameter names (i.e. the names used within the method
  body, not the labels that are part of the method's full name).
- Reordering generic requirements (but not the generic parameters themselves).
- Adding a default argument expression to a parameter.
- Changing or removing a default argument is a `binary-compatible
  source-breaking change`.
- The ``@discardableResult`` and ``@warn_unqualified_access`` attributes may
  be added to a method without any additional versioning information.

Class and instance methods may be marked ``@inlinable``, with the same
restrictions as struct methods. Additionally, only non-overriding ``final``
methods may be marked ``@inlinable``.

.. note::

    A previous draft of this document allowed non-``final`` methods to be
    marked ``@inlinable``, permitting inlining based on speculative
    devirtualization. This was removed because of the added complexity for
    users.


Properties
----------

Class and instance properties allow *most* of the modifications permitted for
struct properties, but the potential for overrides complicates things a little.
Variable properties (those declared with ``var``) allow the following changes:

- Adding (but not removing) a computed setter to a non-``open`` property.
- Adding or removing a non-public, non-versioned setter.
- Changing from a stored property to a computed property, or vice versa, as
  long as a previously versioned setter is not removed.
- Changing the body of an accessor.
- Adding or removing an observing accessor (``willSet`` or ``didSet``) to/from
  an existing variable. This is effectively the same as modifying the body of a
  setter.
- Adding, removing, or changing the initial value of a stored variable.
- Adding or removing ``weak`` from a variable with ``Optional`` type.
- Adding or removing ``unowned`` from a variable.
- Adding or removing ``@NSCopying`` to/from a variable.

Adding a public setter to an ``open`` property is a
`binary-compatible source-breaking change`; any existing overrides will not
know what to do with the setter and will likely not behave correctly.

Constant properties (those declared with ``let``) still permit changing their
value, as well as adding or removing an initial value entirely.

Non-overriding ``final`` variable and constant properties (on both instances
and classes) may be marked ``@inlinableAccess``. This behaves as described for
struct properties.


Subscripts
----------

Subscripts behave much like properties; they inherit the rules of their struct
counterparts with a few small changes:

- Adding (but not removing) a public setter to a non-``open`` subscript is
  permitted.
- Adding or removing a non-public, non-versioned setter is permitted.
- Changing the body of an accessor is permitted.
- Changing index parameter internal names is permitted.
- Reordering generic requirements (but not the generic parameters themselves)
  is permitted.
- Adding a default argument expression to an index parameter is permitted.
- Changing or removing a default argument is a `binary-compatible
  source-breaking change`.

Adding a public setter to an ``open`` subscript is a
`binary-compatible source-breaking change`; any existing overrides will not
know what to do with the setter and will likely not behave correctly.

Non-overriding ``final`` class subscripts may be marked ``@inlinableAccess``,
which behaves as described for struct subscripts.


Possible Restrictions on Classes
--------------------------------

In addition to ``final``, it may be useful to restrict the stored properties of
a class instance, like `Frozen Structs`_. However, there are open
questions about how this would actually work, and the compiler still wouldn't
be able to make much use of the information, because classes from other
libraries must almost always be allocated on the heap.

The design of this annotation is not covered by this document. As a purely
additive feature, it can be added to the model at any time.


Extensions
~~~~~~~~~~

Extensions largely follow the same rules as the types they extend.
The following changes are permitted:

- Adding new extensions and removing empty extensions (that is, extensions that
  declare neither members nor protocol conformances).
- Moving a member from one extension to another within the same module, as long
  as both extensions have the exact same constraints.
- Adding any new member.
- Reordering members.
- Removing any non-public, non-versioned member.
- Changing the body of any methods, initializers, or accessors.

Additionally, non-protocol extensions allow a few additional changes:

- Moving a member from an unconstrained extension to the declaration of the
  base type, provided that the declaration is in the same module. The reverse
  is permitted for all members except stored properties, although note that
  moving all initializers out of a type declaration may cause a new one to be
  implicitly synthesized.
- Adding a new protocol conformance (with proper availability annotations).
- Removing conformances to non-public protocols.

.. note::

    Although it is not related to evolution, it is worth noting that members of
    protocol extensions that do *not* satisfy protocol requirements are not
    overridable, even when the conforming type is a class.


Operators and Precedence Groups
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Operator and precedence group declarations are entirely compile-time
constructs, so changing them does not have any effect on binary compatibility.
However, they do affect *source* compatibility, so it is recommended that
existing operators are not changed at all except for the following:

- Making a non-associative precedence group left- or right-associative.

Any other change counts as a `binary-compatible source-breaking change`.

Operator and precedence group declarations are not versioned.


Typealiases
~~~~~~~~~~~

Public typealiases within structs, enums, and protocols may be used for
protocol conformances (to satisfy associated type requirements), not only
within the library but within client modules as well. Therefore, changing a
member typealias in any way is not permitted; while it will not break existing
clients, they cannot recompile their code and get correct behavior.

Top-level typealiases only exist at compile-time, so changing the underlying
type of one is a `binary-compatible source-breaking change`. However, if the
typealias is *used* in the type of any ABI-public declaration in a library, it
may be an actual breaking change and would not be permitted.

It is always permitted to change the *use* of a public typealias to its
underlying type, and vice versa, at any location in the program.

Typealiases require availability annotations despite being compile-time
constructs in order to verify the availability of their underlying types.


``@usableFromInline``
=====================

Adding ``@usableFromInline`` to an ``internal`` entity promises that the entity
will be available at link time in the containing module's binary. This makes it
safe to refer to such an entity from an inlinable function or in the stored
properties of a frozen struct. ``@usableFromInline`` declarations shipped as
part of an OS should have availability just like ``public`` declarations; if
the entity is ever made ``public`` or ``open``, its availability should not be
changed.

.. note::

    Why isn't ``@usableFromInline`` a special form of ``public``? Because we
    don't want it to imply everything that ``public`` does, such as requiring
    overrides to be ``public``.

Because a ``@usableFromInline`` class member may eventually be made ``open``,
the compiler must assume that new overrides may eventually appear from outside
the module if the class is marked ``open`` unless the member is marked
``final``.


Optimization
============

Allowing a library to evolve inhibits the optimization of client code in
several ways. For example:

- A function that currently does not access global memory might do so in the
  future, so calls to it cannot be freely reordered in client code.

- A stored property may be replaced by a computed property in the future, so
  client code must not try to access the storage directly.

- A struct may have additional members in the future, so client code must not
  assume it fits in any fixed-sized allocation.

If the entity is declared within the same module as the code that's using it,
then the code is permitted to know all the details of how the entity is
declared. After all, if the entity is changed, the code that's using it will be
recompiled. However, if the entity is declared in another module, then the code
using it must be more conservative, and will therefore receive more
conservative answers to its queries. (For example, a stored property may be
treated as computed.)

As a special case, inlinable code must be treated as if it is outside the
current module, since once it's inlined it will be.


Summary
=======

When possible, Swift gives library authors freedom to evolve their code
without breaking binary compatibility. This has implications for both the
semantics and performance of client code, and so library owners also have tools
to waive the ability to make certain future changes. The language guarantees
that client code will never accidentally introduce implicit dependencies on
specific versions of libraries.


Glossary
========

.. glossary::

  ABI
    The run-time contract for using a particular API (or for an entire library),
    including things like symbol names, calling conventions, and type layout
    information. Stands for "Application Binary Interface".

  API
    An `entity` in a library that a `client` may use, or the collection of all
    such entities in a library. (If contrasting with `SPI`, only those entities
    that are available to arbitrary clients.) Marked ``public`` or ``open`` in
    Swift. Stands for "Application Programming Interface".

  availability context
    The collection of library and platform versions that can be assumed, at
    minimum, to be present in a certain block of code. Availability contexts
    are always properly nested, and the global availability context includes
    the module's minimum deployment target and minimum dependency versions.

  backwards-compatible
    A modification to an API that does not break existing clients. May also
    describe the API in question.

  binary compatibility
    A general term encompassing both backwards- and forwards-compatibility
    concerns. Also known as "ABI compatibility".

  binary-compatible source-breaking change
    A change that does not break `binary compatibility`, but which is known to
    either change the behavior of existing clients or potentially result in
    errors when a client is recompiled. In most cases, a client that *hasn't*
    been recompiled may use the new behavior or the old behavior, or even a
    mix of both; however, this will always be deterministic (same behavior when
    a program is re-run) and will not break Swift's memory-safety and
    type-safety guarantees. It is recommended that these kinds of changes are
    avoided just like those that break binary compatibility.

  client
    A target that depends on a particular library. It's usually easiest to
    think of this as an application, but it could be another library.
    (In certain cases, the "library" is itself an application, such as when
    using Xcode's unit testing support.)

  duck typing
    In Objective-C, the ability to treat a class instance as having an
    unrelated type, as long as the instance handles all messages sent to it.
    (Note that this is a dynamic constraint.)

  entity
    A type, function, member, or global in a Swift program. Occasionally the
    term "entities" also includes conformances, since these have a run-time
    presence and are depended on by clients.

  forwards-compatible
    An API that is designed to handle future clients, perhaps allowing certain
    changes to be made without changing the ABI.

  fragility attribute
    See `A Unifying Theme`_.

  module
    The primary unit of code sharing in Swift. Code in a module is always built
    together, though it may be spread across several source files.

  performance assertion
    See `Other Promises About Types`_.

  resilience domain
    A grouping for code that will always be recompiled and distributed
    together, and can thus take advantage of details about a type
    even if it changes in the future.

  SPI
    A subset of `API` that is only available to certain clients. Stands for
    "System Programming Interface".

  target
    In this document, a collection of code in a single Swift module that is
    built together; a "compilation unit". Roughly equivalent to a target in
    Xcode.

  versioned entity
    See `Publishing Versioned API`_.

  versioned attribute
    See `Publishing Versioned API`_.
