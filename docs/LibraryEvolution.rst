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
writing a bundled library should not be required to use any of the annotations
described below in order to achieve full performance.

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


Using Versioned API
===================

References to a versioned API must always be guarded with the appropriate
availability checks. This means that any client entities that rely on certain
APIs from a library must themselves be restricted to contexts in which those
APIs are available. This is accomplished using the ``@available`` attribute, by
specifying the name of the client library along with the required version::

    // Client code
    @available(Magician 1.5)
    class CrystalBallView : MagicView { /*...*/ }

Library versions can also be checked dynamically using ``#available``, allowing
for fallback behavior when the requested library version is not present::

    func scareMySiblings() {
      if #available(Magician 1.2) {
        summonDemons()
      } else {
        print("BOO!!")
      }
    }

.. note::

    Possible implementations include generating a hidden symbol into a library,
    or putting the version number in some kind of metadata, like the Info.plist
    in a framework bundle on Darwin platforms.

This is essentially the same model as the availability checking released in
Swift 2.0, but generalized for checking library versions instead of just OS
versions.


Declaring Library Version Dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Swift's current availability model includes the notion of a *minimum deployment
target,* the version of an OS that must be present for the program being
compiled to run at all. For example, a program compiled with a minimum
deployment target of iOS 9.2 will not launch on iOS 9.0.

The generalized model above suggests being able to make similar guarantees for
individual libraries. For example, a client program may depend on version 1.1
of the "Magician" library; trying to run using version 1.0 will result in
errors. By declaring this at compile-time, the client code can omit
``@available`` and ``#available`` checks that are satisfied by the minimum
library version.

Both the syntax and enforcement of this feature are not covered by this
document.


Publishing Versioned API
========================

A library's API is already marked with the ``public`` modifier, but if a
client wants to work with multiple releases of the library, the API needs
versioning information as well. A *versioned entity* represents anything with a
run-time presence that a client may rely on; its version records when the entity
was first exposed publicly in its library. Put another way, it is the oldest
version of the library where the entity may be used.

- Classes, structs, enums, and protocols may all be versioned entities.
- Methods, properties, subscripts, and initializers may be versioned entities.
- Top-level functions, variables, and constants may be versioned entities.
- Protocol conformances may be versioned entities, despite not explicitly having
  a declaration in Swift, because a client may depend on them.
  See `New Conformances`_, below.
- Typealiases are treated as versioned entities for the purpose of verifying
  availability, even though they have no run-time presence.

In a versioned library, any top-level public entity from the list above may not
be made ``public`` (or ``open``) without an appropriate version. A public
entity declared within a versioned type (or an extension of a versioned type)
will default to having the same version as the type.

In this document, the term "public" includes classes and members marked
``open``.

Code within a library may generally use all other entities declared within the
library (barring their own availability checks), since the entire library is
shipped as a unit. That is, even if a particular API was introduced in v1.0,
its (non-public) implementation may refer to APIs introduced in later versions.

Certain uses of ``internal`` entities require them to be part of a library's
binary interface, which means they need to be versioned as well. See
`Versioning Internal Declarations`_ below.

In addition to versioned entities, there are also attributes that are safe to
add to declarations when releasing a new version of a library. In most cases,
clients can only take advantage of the attributes when using the new release of
the library, and therefore the attributes also need to record the version in
which they were introduced; these are called *versioned attributes.* If the
version is omitted, it is assumed to be the version of the declaration to which
the attribute is attached.

The syntax for marking an entity as versioned has not yet been decided, but the
rest of this document will use syntax #1 described below.


Syntax #1: Attributes
~~~~~~~~~~~~~~~~~~~~~

::

    @available(1.2)
    public func summonDemons()

    @available(1.0) @inlinable(1.2)
    public func summonElves()

Using the same attribute for both publishing and using versioned APIs helps tie
the feature together and enforces a consistent set of rules. However, there are
several other annotations described later in this document that also need
versioning information, and it may not be obvious what the version number means
outside the context of ``available``.


Syntax #2: Version Blocks
~~~~~~~~~~~~~~~~~~~~~~~~~

::

    #version(1.2)
    public func summonDemons()

    #version(1.0) {}
    #version(1.2) { @inlinable }
    public func summonElves()

Since there are potentially many annotations on a declaration that need
versioning information, it may make sense to group them together in some way.
Only certain annotations would support being versioned in this way.


Syntax #3: The ``public`` modifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    public(1.2) func summonDemons()

    /* @inlinable ?? */
    public(1.0) func summonElves()

Putting the version on the public modifier is the most concise option. However,
there's no obvious syntax here for adding versions to other annotations that
may apply to a declaration.

(Also, at one point there was a proposal to tag API only intended for certain
clients using a similar syntax: ``public("Foundation")``, for example, for APIs
only meant to be used by Foundation. These could then be stripped out of the
public interface for a framework before being widely distributed. But that
could easily use an alternate syntax.)


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

- The function accesses a fixed-contents struct with non-public members; this
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

Any local functions or closures within an inlinable function are themselves
treated as ``@inlinable``, and a client that inlines the containing function
must emit its own copy of the local functions or closures. This is important in
case it is necessary to change the inlinable function later; existing clients
should not be depending on internal details of the previous implementation.

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


Default Argument Expressions
----------------------------

Default argument expressions for functions that are public, versioned, or
inlinable are implemented very similar to inlinable functions and thus are
subject to similar restrictions:

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


Fixed-Contents Structs
----------------------

To opt out of this flexibility, a struct may be marked ``@fixedContents``.
This promises that no stored properties will be added to or removed from the
struct, even non-public ones. Additionally, all versioned instance stored
properties in a ``@fixedContents`` struct are implicitly declared
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
    ``@fixedContents`` structs. Static members effectively behave as top-level
    functions and variables.

.. note::

    The name ``@fixedContents`` is intentionally awful to encourage us to come
    up with a better one.

While adding or removing stored properties is forbidden, existing properties may
still be modified in limited ways:

- An existing non-public, non-versioned property may change its access level to
  any other non-public access level.
- A non-versioned ``internal`` property may be versioned (see `Versioning
  Internal Declarations`_).
- A versioned ``internal`` property may be made ``public`` (without changing
  its version).

An initializer of a fixed-contents struct may be declared ``@inlinable`` even
if it does not delegate to another initializer, as long as the ``@inlinable``
attribute, or the initializer itself, is not introduced earlier than the
``@fixedContents`` attribute and the struct has no non-versioned stored
properties.

A ``@fixedContents`` struct is *not* guaranteed to use the same layout as a C
struct with a similar "shape". If such a struct is necessary, it should be
defined in a C header and imported into Swift.

.. note::

    We can add a *different* feature to control layout some day, or something
    equivalent, but this feature should not restrict Swift from doing useful
    things like minimizing member padding. At the very least, Swift structs
    don't guarantee the same tail padding that C structs do.

.. note::

    Hypothetically, we could use a different model where a ``@fixedContents``
    struct only guarantees the "shape" of the struct, so to speak, while
    leaving all property accesses to go through function calls. This would
    allow stored properties to change their accessors, or (with the Behaviors
    proposal) to change a behavior's implementation, or change from one
    behavior to another. However, the *most common case* here is probably just
    a simple C-like struct that groups together simple values, with only public
    stored properties and no observing accessors, and having to opt into direct
    access to those properties seems unnecessarily burdensome. The struct is
    being declared ``@fixedContents`` for a reason, after all: it's been
    discovered that its use is causing performance issues.

    Consequently, as a first pass we may just require all stored properties in
    a ``@fixedContents`` struct, public or non-public, to have trivial
    accessors, i.e. no observing accessors and no behaviors.

``@fixedContents`` is a `versioned attribute`. This is so that clients can
deploy against older versions of the library, which may have a different layout
for the struct. (In this case the client must manipulate the struct as if the
``@fixedContents`` attribute were absent.)


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
a class instance, like `Fixed-Contents Structs`_. However, there are open
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
constructs, so changing them does not have any affect on binary compatibility.
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
typealias is *used* in the type of any `versioned entity` in a library, it
may be an actual breaking change and would not be permitted.

It is always permitted to change the *use* of a public typealias to its
underlying type, and vice versa, at any location in the program.

Typealiases are `versioned <versioned entity>` despite being compile-time
constructs in order to verify the availability of their underlying types.


A Unifying Theme
~~~~~~~~~~~~~~~~

So far this document has talked about ways to give up flexibility for several
different kinds of declarations: ``@inlinable`` for functions,
``@fixedContents`` for structs, etc. Each of these has a different set of
constraints it enforces on the library author and promises it makes to clients.
However, they all follow a common theme of giving up the flexibility of future
changes in exchange for improved performance and perhaps some semantic
guarantees. Therefore, all of these attributes are informally referred to as
"fragility attributes".

Given that these attributes share several characteristics, we could consider
converging on a single common attribute, say ``@fixed``, ``@inline``, or
``@fragile``. However, this may be problematic if the same declaration has
multiple kinds of flexibility.


Versioning Internal Declarations
================================

The initial discussion on versioning focused on public APIs, making sure
that a client knows what features they can use when a specific version of a
library is present. Inlinable functions have much the same constraints, except
the inlinable function is the client and the entities being used may not be
public.

Adding a versioning annotation to an ``internal`` entity promises that the
entity will be available at link time in the containing module's binary. This
makes it safe to refer to such an entity from an inlinable function. If the
entity is ever made ``public`` or ``open``, its availability should not be
changed; not only is it safe for new clients to rely on it, but *existing*
clients require its presence as well.

.. note::

    Why isn't this a special form of ``public``? Because we don't want it to
    imply everything that ``public`` does, such as requiring overrides to be
    ``public``.

In libraries without binary compatibility concerns, the equivalent annotation
is ``@usableFromInline``, since inlinable functions are the only way that a
non-public entity can be referenced from outside of a module.

Because a versioned class member may eventually be made ``open``, it must be
assumed that new overrides may eventually appear from outside the module if the
class is marked ``open`` unless the member is marked ``final``.

Non-public conformances are never considered versioned, even if both the
conforming type and the protocol are versioned. A conformance is considered
public if and only if both the conforming type and protocol are public.

Entities declared ``private`` or ``fileprivate`` may not be versioned; the
mangled name of such an entity includes an identifier based on the containing
file, which means moving the declaration to another file changes the entity's
mangled name. This implies that a client would not be able to find the entity
at run time if the source code is reorganized, which is unacceptable.

.. note::

    There are ways around this limitation, the most simple being that versioned
    ``private`` entities are subject to the same cross-file redeclaration rules
    as ``internal`` entities. However, this is a purely additive feature, so to
    keep things simple we'll stick with the basics.

We could do away with the entire feature if we restricted inlinable functions
and fixed-contents structs to only refer to public entities. However, this
removes one of the primary reasons to make something inlinable: to allow
efficient access to a type while still protecting its invariants.


"Backdating"
============

*Backdating* refers to releasing a new version of a library that contains
changes, but pretending those changes were made in a previous version of the
library. For example, you might want to release version 1.2 of the "Magician"
library, but pretend that the "SpellIncantation" struct was fixed-contents
since its introduction in version 1.0.

**This is not safe.**

Backdating the availability a versioned entity that was previously non-public
is clearly not safe: older versions of the library will not expose the entity
as part of their ABI. What may be less obvious is that the fragility attributes
likewise are not safe to backdate, even if you know the attributes could have
been added in the past. To give one example, the presence of ``@closed`` or
``@fixedContents`` may affect the layout and calling conventions for an enum
or struct.

.. note::

    If we add an "SPI" feature, such that the use of specific public entities
    is limited to certain clients, it *will* be safe to change the set of
    clients, or remove the restriction altogether. In fact, in such cases the
    library author is *required* to *not* change the availability info that was
    originally presented for the limited set of clients, since as mentioned
    above this may affect how those existing clients use the entities declared
    in the library.

The one exception is ``@inlinable``, which does not change how a function is
called or otherwise used at the ABI level. If the implementation being provided
is compatible with a previous version of a library, and the function was
present and public (or `versioned <versioned entity>`) there, then the library
author may choose to backdate the ``@inlinable`` annotation.


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

In order to make sure client code doesn't make unsafe assumptions, queries
about properties that may change between library versions must be parameterized
with the `availability context` that is using the entity. An availability
context is a set of minimum platform and library versions that can be assumed
present for code executing within the context. (See `Declaring Library Version
Dependencies`_.) This allows the compiler to answer the question, "Given what I
know about where this code will be executed, what can I assume about a
particular entity being used?".

If the entity is declared within the same module as the code that's using it,
then the code is permitted to know all the details of how the entity is
declared. After all, if the entity is changed, the code that's using it will be
recompiled.

However, if the entity is declared in another module, then the code using it
must be more conservative, and will therefore receive more conservative answers
to its queries. For example, a stored property may report itself as computed.

The presence of versioned fragility attributes makes the situation more
complicated. Within a client function that requires version 1.5 of a particular
library, the compiler should be able to take advantage of any fragility
information (and performance assertions) introduced prior to version 1.5.


Inlinable Code
~~~~~~~~~~~~~~

By default, the availability context for a library always includes the latest
version of the library itself, since that code is always distributed as a unit.
However, this is not true for functions that have been marked inlinable (see
`Inlinable Functions`_ above). Inlinable code must be treated as if it is
outside the current module, since once it's inlined it will be.

For inlinable code, the availability context is exactly the same as the
equivalent non-inlinable code except that the assumed version of the
containing library is the version attached to the ``@inlinable`` attribute, or
the version of the library in which the entity was introduced, and any `library
version dependencies <#declaring-library-version-dependencies>`_ or minimum
deployment target must be specified explicitly using ``@available``. Code
within this context must be treated as if the containing library were just a
normal dependency.

A versioned inlinable function still has an exported symbol in the library
binary, which may be used when the function is referenced from a client rather
than called. This version of the function is not subject to the same
restrictions as the version that may be inlined, and so it may be desirable to
compile a function twice: once for inlining, once for maximum performance.

If the body of an inlinable function is used in any way by a client module
(say, to determine that it does not read any global variables), that module
must take care to emit and use its own copy of the function. This is because
analysis of the function body may not apply to the version of the function
currently in the library.


Local Availability Contexts
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Swift availability contexts aren't just at the declaration level; they also
cover specific regions of code inside function bodies as well. These "local"
constructs are formed using the ``#available`` construct, which performs a
dynamic check.

In theory, it would be legal to allow code dominated by a ``#available`` check
to take advantage of additional fragility information introduced by the more
restrictive dependencies that were checked for. However, this is an additional
optimization that may be complicated to implement (and even to represent
properly in SIL), and so it is not a first priority.


Other Promises About Types
~~~~~~~~~~~~~~~~~~~~~~~~~~

Advanced users may want to promise more specific things about various types.
These are similar to the internal ``effects`` attribute we have for functions,
except that they can be enforced by the compiler.

- ``trivial``: Promises that assignment just requires a fixed-size bit-for-bit
  copy without any indirection or reference-counting operations.

- ``maximumFootprint(sizeInBits: N, alignmentInBits: A)``: Promises that the
  type's size and required alignment are at most N bits and A bits,
  respectively. (Both may be smaller.)

- ``fixedSize``: Promises that the type has *some* size known at compile-time,
  allowing optimizations like promoting allocations to the stack. Only applies
  to fixed-contents structs and closed enums, which can already infer this
  information; the explicit annotation allows it to be enforced.

Collectively these features are known as "performance assertions", to
underscore the fact that they do not affect how a type is used at the source
level, but do allow for additional optimizations. We may also expose some of
these qualities to static or dynamic queries for performance-sensitive code.

.. note:: Previous revisions of this document contained a ``noPayload``
    assertion for enums. However, this doesn't actually offer any additional
    optimization opportunities over combining ``trivial`` with
    ``maximumFootprint``, and the latter is more flexible.

.. note:: None of these names / spellings are final. The name "trivial" comes
    from C++, though Swift's trivial is closer to C++'s "`trivially
    copyable`__".

All of these features need to be versioned, just like the more semantic
fragility attributes above. The exact spelling is not proposed by this document.

__ http://en.cppreference.com/w/cpp/types/is_trivially_copyable


Resilience Domains
==================

As described in the `Introduction`_, the features and considerations discussed
in this document do not apply to libraries distributed in a bundle with their
clients. In this case, a client can rely on all the current implementation
details of its libraries when compiling, since the same version of the library
is guaranteed to be present at run time. This allows more optimization than
would otherwise be possible.

In some cases, a collection of libraries may be built and delivered together,
even though their clients may be packaged separately. (For example, the ICU
project is usually built into several library binaries, but these libraries are
always distributed together.) While the *clients* cannot rely on a particular
version of any library being present, the various libraries in the collection
should be able to take advantage of the implementations of their dependencies
also in the collection---that is, it should treat all entities as if marked
with the appropriate fragility attributes. Modules in this sort of collection
are said to be in the same *resilience domain.*

Exactly how resilience domains are specified is not covered by this document,
and indeed they are an additive feature. One possibility is that a library's
resilience domain defaults to the name of the module, but can be overridden. If
a client has the same resilience domain name as a library it is using, it may
assume that version of the library will be present at run time.


Deployments
~~~~~~~~~~~

Related to the concept of a resilience domain is a *deployment.* While a
resilience domain allows related libraries to be compiled more efficiently,
a deployment groups related libraries together to present semantic version
information to clients. The simplest example of this might be an OS release:
OS X 10.10.0 contains Foundation version 1151.16 and AppKit version 1343. A
deployment thus acts as a "virtual dependency": clients that depend on
OS X 10.10 can rely on the presence of both of the library versions above.

The use of deployments allows clients to only have to think about aggregate
dependencies, instead of listing every library they might depend on. It also
allows library authors to build `many versions of a library`__ within a larger
release cycle, as well as allowing a vendor to bundle together many libraries
with uncoordinated release schedules and release them as a logical unit.

__ https://developer.apple.com/library/ios/documentation/Cocoa/Reference/Foundation/Miscellaneous/Foundation_Constants/index.html#//apple_ref/doc/constant_group/Foundation_Framework_Version_Numbers

There are lots of details to figure out here, including how to distribute this
information. In particular, just like libraries publish the history of their
own APIs, a deployment must publish the history of their included library
versions, i.e. not just that OS X 10.10 contains Foundation 1151.16 and AppKit
1343, but also that OS X 10.9 contains Foundation 1056 and AppKit 1265, and that
OS X 10.8 contains Foundation 945.0 and AppKit 1187, and so on, back to the
earliest version of the deployment that is supported.



Checking Binary Compatibility
=============================

With this many manual controls, it's important that library owners be able to
check their work. Therefore, we intend to build a tool that can compare two
versions of a library's public interface, and present any suspect differences
for verification. Important cases include but are not limited to:

- Removal of versioned entities.

- Incompatible modifications to versioned entities, such as added protocol
  conformances lacking versioning information.

- Unsafe `backdating <#backdating>`_.

- Unsafe modifications to entities marked with fragility attributes, such as
  adding a stored property to a ``@fixedContents`` struct.

Wherever possible, this tool should also check for `binary-compatible
source-breaking changes <binary-compatible source-breaking change>`, such as
changing a default argument from ``false`` to ``true``.


Automatic Versioning
~~~~~~~~~~~~~~~~~~~~

A possible extension of this "checker" would be a tool that *automatically*
generates versioning information for entities in a library, given the previous
public interface of the library. This would remove the need for versions on any
of the fragility attributes, and declaring versioned API would be as simple as
marking an entity ``public``. Obviously this would also remove the possibility
of human error in managing library versions.

However, making this tool has a number of additional difficulties beyond the
simple checker tool:

- The tool must be able to read past library interface formats. This is true
  for a validation tool as well, but the cost of failure is much higher.
  Similarly, the past version of a library *must* be available to correctly
  compile a new version.

- Because the information goes into a library's public interface, the
  versioning tool must either be part of the compilation process, modify the
  interface generated by compilation, or produce a sidecar file that can be
  loaded when compiling the client. In any case, it must *produce* information
  in addition to *consuming* it.

- Occasionally a library owner may want to override the inferred versions. This
  can be accomplished by providing explicit versioning information, as
  described above.

- Bugs in the tool manifest as bugs in client programs.

Because this tool would require a fair amount of additional work, it is not
part of this initial model. It is something we may decide to add in the future.


Open Issues
===========

There are still a number of known issues with the model described in this
document. We should endeavor to account for each of them, and if we can't come
up with a satisfactory implementation we should at least make sure that they
will not turn into pitfalls for library or client developers.


Subclass and base both conform to protocol
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    // Library, version 1
    class Elf {}
    protocol Summonable {}

::

    // Client, version 1
    class ShoemakingElf : Elf, Summonable {}

::

    // Library, version 2
    @available(2.0)
    extension Elf : Summonable {}

Now ``ShoemakingElf`` conforms to ``Summonable`` in two different ways, which
may be incompatible (especially if ``Summonable`` had associated types or
requirements involving ``Self``).

Additionally, the client can't even remove ``ShoemakingElf``'s conformance to
``Summonable``, because it may itself be a library with other code depending on
it. We could fix that with an annotation to explicitly inherent the conformance
of ``Summonable`` from the base class, but even that may not be possible if
there are incompatible associated types involved (because changing a member
typealias is not a safe change).

One solution is to disallow adding a conformance for an existing protocol to an
``open`` class.


Recompiling changes a protocol's implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    // Library, version 1
    protocol MagicType {}
    protocol Wearable {}
    func use<T: MagicType>(_ item: T) {}

::

    // Client, version 1
    struct Amulet : MagicType, Wearable {}
    use(Amulet())

::

    // Library, version 2
    protocol MagicType {
      @available(2.0)
      func equip() { print("Equipped.") }
    }

    extension Wearable where Self: MagicType {
      @available(2.0)
      func equip() { print("You put it on.") }
    }

    func use<T: MagicType>(_ item: T) { item.equip() }

Before the client is recompiled, the implementation of ``equip()`` used for
``Amulet`` instances can only be the default implementation, i.e. the one that
prints "Equipped". However, recompiling the client will result in the
constrained implementation being considered a "better" match for the protocol
requirement, thus changing the behavior of the program.

This should never change the *meaning* of a program, since the default
implementation for a newly-added requirement should always be *correct.*
However, it may have significantly different performance characteristics or
side effects that would make the difference in behavior a surprise.

This is similar to adding a new overload to an existing set of functions, which
can also change the meaning of client code just by recompiling. However, the
difference here is that the before-recompilation behavior was never requested
or acknowledged by the client; it's just the best the library can do.

A possible solution here is to require the client to acknowledge the added
requirement in some way when it is recompiled.

(We do not want to perform overload resolution at run time to find the best
possible default implementation for a given type.)


Summary
=======

When possible, Swift gives library authors freedom to evolve their code
without breaking binary compatibility. This has implications for both the
semantics and performance of client code, and so library owners also have tools
to waive the ability to make certain future changes. The language guarantees
that client code will never accidentally introduce implicit dependencies on
specific versions of libraries.


Related Proposals
=================

The following proposals (some currently in the process, some planned) will
affect the model described in this document, or concern the parts of this
document that affect language semantics:

- (draft) `Overridable methods in extensions`_
- (planned) Restricting retroactive modeling (protocol conformances for types you don't own)
- (planned) `Generalized existentials (values of protocol type) <Generics>`_
- (planned) Frozen enums (building on `SE-0192 <SE0192>`_)
- (planned) Removing the "constant" guarantee for 'let' across module boundaries
- (planned) Syntax for declaring fixed-contents structs
- (future) Performance annotations for types
- (future) Attributes for stored property accessors
- (future) Stored properties in extensions

.. _Overridable methods in extensions: https://github.com/jrose-apple/swift-evolution/blob/overridable-members-in-extensions/proposals/nnnn-overridable-members-in-extensions.md
.. _Generics: https://github.com/apple/swift/blob/master/docs/GenericsManifesto.md#generalized-existentials
.. _SE0192: https://github.com/apple/swift-evolution/blob/master/proposals/0192-non-exhaustive-enums.md

This does not mean all of these proposals need to be accepted, only that their
acceptance or rejection will affect this document.


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
