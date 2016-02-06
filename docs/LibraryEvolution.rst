:orphan:

.. default-role:: term
.. title:: Library Evolution Support in Swift ("Resilience")

:Author: Jordan Rose
:Author: John McCall

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

.. warning:: **This document is still in draft stages.** Large additions and
  restructuring are still planned, including:

  * A discussion of back-dating, and how it usually is not allowed.
  * A brief discussion of "deployment files", which represent distribution groupings that are themselves versioned. (For example, OS X 10.10.3 contains Foundation version 1153.20.) Deployment files are likely to provide a concrete implementation of "resilience domains".

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

This model is not of interest to libraries that are bundled with their clients
(distribution via source, static library, or embedded/sandboxed dynamic
library). Because a client always uses a particular version of such a library,
there is no need to worry about backwards- or forwards-compatibility. Just as
developers with a single app target are not forced to think about access
control, anyone writing a bundled library should not be required to use any of
the annotations described below in order to achieve full performance.

The term "resilience" comes from the occasional use of "fragile" to describe
certain constructs that have very strict binary compatibility rules. For
example, a client's use of a C struct is "fragile" in that if the library
changes the fields in the struct, the client's use will "break". In Swift,
changing the fields in a struct will not automatically cause problems for
existing clients, so we say the struct is "resilient".


Using Versioned API
===================

References to a versioned API must always be guarded with the appropriate
availability checks. This means that any client entities that rely on having
certain APIs from a library must themselves be restricted to contexts in which
those APIs are available. This is accomplished using ``@available`` as well,
by specifying the name of the client library along with the required version::

    // Client code
    @available(Magician 1.5)
    class CrystalBallView : MagicView { … }

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

Swift 2's availability model includes the notion of a *minimum deployment
target,* the version of an OS that can be assumed present for the program being
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

A library's API is already marked with the ``public`` attribute, but if a
client wants to work with multiple releases of the library, the API needs
versioning information as well. A *versioned entity* represents anything with a
runtime presence that a client may rely on; its version records when the entity
was first exposed publicly in its library. Put another way, it is the oldest
version of the library where the entity may be used.
  
- Classes, structs, enums, and protocols may all be versioned entities.
- Methods, properties, subscripts, and initializers may be versioned entities.
- Top-level functions, variables, and constants may be versioned entities.
- Protocol conformances may be versioned entities, despite not explicitly having
  a declaration in Swift, because a client may depend on them
  See `New Conformances`_, below.

In a versioned library, any top-level public entity from the list above may not
be made ``public`` without an appropriate version. A public entity declared
within a versioned type (or an extension of a versioned type) will default to
having the same version as the type.

Code within a library may generally use all other entities declared within the
library (barring their own availability checks), since the entire library is
shipped as a unit. That is, even if a particular API was introduced in v1.0,
its (non-public) implementation may refer to APIs introduced in later versions.

Swift libraries are strongly encouraged to use `semantic versioning`_, but this
is not enforced by the language.

.. _semantic versioning: http://semver.org

Certain uses of ``internal`` entities require them to be part of a library's
binary interface, which means they need to be versioned as well. See
`Versioning Internal Declarations`_ below.

In addition to versioned entities, there are also attributes that are safe to
add to declarations when releasing a new version of a library. In most cases,
clients can only take advantage of the attributes when using the new release of
the library, and therefore the attributes also need to record the version in
which they were introduced. If the version is omitted, it is assumed to be the
version of the declaration to which the attribute is attached.

The syntax for marking an entity as versioned has not yet been decided, but the
rest of this document will use syntax #1 described below.


Syntax #1: Attributes
~~~~~~~~~~~~~~~~~~~~~

::

    @available(1.2)
    public func summonDemons()

    @available(1.0) @inlineable(1.2)
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
    #version(1.2) { @inlineable }
    public func summonElves()

Since there are potentially many annotations on a declaration that need
versioning information, it may make sense to group them together in some way.
Only certain annotations would support being versioned in this way.


Syntax #3: The ``public`` modifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    public(1.2) func summonDemons()

    /* @inlineable ?? */
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
- Adding a default value to a parameter.
- Changing or removing a default value is permitted but discouraged; it may
  break or change the meaning of existing source code.
- The ``@noreturn`` attribute may be added to a function. ``@noreturn`` is a
  `versioned attribute`.
- The ``@warn_unused_result`` and ``@warn_unqualified_access`` attributes may
  be added to a function without any additional versioning information.

.. note::

    Swift 2's implementation of default values puts the evaluation of the
    default value expression in the library, rather than in the client like C++
    or C#. We plan to change this.

No other changes are permitted; the following are particularly of note:

- A versioned function may not change its parameters or return type.
- A versioned function may not change its generic requirements.
- A versioned function may not change its external parameter names (labels).
- A versioned function may not add, remove, or reorder parameters, whether or
  not they have default values.
- A versioned function that throws may not become non-throwing.
- ``@noreturn`` may not be removed from a function.
- The ``@noescape`` attribute may not be added to or removed from a parameter.
  It is not a `versioned attribute` and so there is no way to guarantee that it
  is safe when a client deploys against older versions of the library.


Inlineable Functions
--------------------

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

A versioned function marked with the ``@inlineable`` attribute makes its body
available to clients as part of the module's public interface. ``@inlineable``
is a `versioned attribute`; clients may not assume that the body of the
function is suitable when deploying against older versions of the library.

Clients are not required to inline a function marked ``@inlineable``.

.. note::

    It is legal to change the implementation of an inlineable function in the
    next release of the library. However, any such change must be made with the
    understanding that it may or may not affect existing clients. Existing
    clients may use the new implementation, or they may use the implementation
    from the time they were compiled, or they may use both inconsistently.


Restrictions on Inlineable Functions
------------------------------------

Because the body of an inlineable function (or method, accessor, initializer,
or deinitializer) may be inlined into another module, it must not make any
assumptions that rely on knowledge of the current module. Here is a trivial
example using methods::

    public struct Point2D {
      var x, y: Double
      public init(x: Double, y: Double) { … }
    }

    extension Point2D {
      @inlineable public func distanceTo(other: Point2D) -> Double {
        let deltaX = self.x - other.x
        let deltaY = self.y - other.y
        return sqrt(deltaX*deltaX + deltaY*deltaY)
      }
    }

As written, this ``distanceTo`` method is not safe to inline. The next release
of the library could very well replace the implementation of ``Point2D`` with a
polar representation::

    public struct Point2D {
      var r, theta: Double
      public init(x: Double, y: Double) { … }
    }

and the ``x`` and ``y`` properties have now disappeared. To avoid this, we have
the following restrictions on the bodies of inlineable functions:

- **They may not define any local types** (other than typealiases).

- **They must not reference any** ``private`` **entities,** except for local
  functions declared within the inlineable function itself.

- **They must not reference any** ``internal`` **entities except for those that
  have been** `versioned`_. See below for a discussion of versioning internal
  API.

- **They must not reference any entities less available than the function
  itself.**

.. _versioned: #versioning-internal-api

An inlineable function is still emitted into its own module's binary. This
makes it possible to take an existing function and make it inlineable, as long
as the current body makes sense when deploying against an earlier version of
the library.

If the body of an inlineable function is used in any way by a client module
(say, to determine that it does not read any global variables), that module
must take care to emit and use its own copy of the function. This is because
analysis of the function body may not apply to the version of the function
currently in the library.

Default parameter value expressions are subject to the same restrictions as
inlineable functions, because they are implemented as inlineable functions.

Local Functions
---------------

If an inlineable function contains local functions or closures, these are
implicitly made inlineable as well. This is important in case the library
author decides to change the inlineable function later. If the inlineable
function is emitted into a client module as described above, the local
functions must be as well. (At the SIL level, these local functions are
considered to have ``shared`` linkage.)

Local functions are subject to the same restrictions as the inlineable
functions containing them, as described above.


Top-Level Variables and Constants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a versioned module-scope variable declared with ``var``, the following
changes are permitted:

- Adding (but not removing) a public setter to a computed variable.
- Adding or removing a non-public, non-versioned setter.
- Changing from a stored variable to a computed variable, or vice versa, as
  long as a previously-versioned setter is not removed.
- Changing the body of an accessor.
- Adding or removing an observing accessor (``willSet`` or ``didSet``) to/from
  an existing variable. This is effectively the same as modifying the body of a
  setter.
- Changing the initial value of a stored variable.
- Adding or removing ``weak`` from a variable with ``Optional`` type.
- Adding or removing ``unowned`` from a variable.
- Adding or removing ``@NSCopying`` to/from a variable.

.. admonition:: TODO

    We need to pin down how this interacts with the "Behaviors" proposal.
    Behaviors that just change the accessors of a global are fine, but those
    that provide new entry points are trickier.

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

.. note:: We could make it safe to turn a read-only variable into a constant,
    but currently do not promise that that is a binary-compatible change.


Giving Up Flexibility
---------------------

Both top-level constants and variables can be marked ``@inlineable`` to allow
clients to access them more efficiently. This restricts changes a fair amount:

- Adding a versioned setter to a computed variable is still permitted.
- Adding or removing a non-public, non-versioned setter is still permitted.
- Changing from stored to computed or vice versa is forbidden, because it would
  break existing clients.
- Changing the body of an accessor is permitted but discouraged; existing
  clients may use the new implementations, or they may use the implementations
  from the time they were compiled, or a mix of both.
- Adding/removing observing accessors is likewise permitted but discouraged,
  for the same reason.
- Changing the initial value of a stored variable is still permitted.
- Changing the value of a constant is permitted but discouraged; like accessors,
  existing clients may use the new value, or the value from when they were
  compiled, or a mix of both.
- Adding or removing ``weak`` is forbidden.
- Adding or removing ``unowned`` is forbidden.
- Adding or removing ``@NSCopying`` to/from a variable is permitted but
  discouraged for the same reason.

.. admonition:: TODO

    It Would Be Nice(tm) to allow marking the *getter* of a top-level variable
    inlineable while still allowing the setter to change. This would need
    syntax, though.

Any inlineable accessors must follow the rules for `inlineable functions`_,
as described above.

Note that if a constant's initial value expression has any observable side
effects, including the allocation of class instances, it must not be treated
as inlineable. A constant must always behave as if it is initialized exactly
once.

.. admonition:: TODO

    Is this a condition we can detect at compile-time? Do we have to be
    restricted to things that can be lowered to compile-time constants?


Structs
~~~~~~~

Swift structs are a little more flexible than their C counterparts. By default,
the following changes are permitted:

- Reordering any existing members, including stored properties.
- Adding any new members, including stored properties.
- Changing existing properties from stored to computed or vice versa.
- Changing the body of any methods, initializers, or accessors.
- Adding or removing an observing accessor (``willSet`` or ``didSet``) to/from
  an existing property. This is effectively the same as modifying the body of a
  setter.
- Removing any non-public, non-versioned members, including stored properties.
- Adding a new protocol conformance (with proper availability annotations).
- Removing conformances to non-public protocols.

The important most aspect of a Swift struct is its value semantics, not its
layout.

.. admonition:: TODO

    We need to pin down how this, and the ``@fixed_contents`` attribute below,
    interacts with the "Behaviors" proposal. Behaviors that just change the
    accessors of a property are fine, but those that provide new entry points
    are trickier.

Like top-level constants, it is *not* safe to change a ``let`` property into a
variable or vice versa. Properties declared with ``let`` are assumed not to
change for the entire lifetime of the program once they have been initialized.

It is not safe to add or remove ``mutating`` or ``nonmutating`` from a member
or accessor within a struct. These modifiers are not `versioned attributes
<versioned attribute>` and as such there is no safety guarantee for a client
deploying against an earlier version of the library.


New Conformances
----------------

If a conformance is added to a type in version 1.1 of a library, it's important
that it isn't accessed in version 1.0. This is obvious if the protocol itself
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
    extension MyStruct : SomeProto {…}

and

::

    extension MyStruct : @available(1.1) SomeProto {…}

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

To opt out of this flexibility, a struct may be marked ``@fixed_contents``.
This promises that no stored properties will be added to or removed from the
struct, even ``private`` or ``internal`` ones. Additionally, all versioned
stored properties in a ``@fixed_contents`` struct are implicitly declared
``@inlineable`` (as described above for top-level variables). In effect:

- Reordering all members, including stored properties, is still permitted.
- Adding new stored instance properties (public or non-public) is not permitted.
  Adding any other new members is still permitted.
- Existing instance properties may not be changed from stored to computed or
  vice versa.
- Changing the body of any *existing* methods, initializers, computed property
  accessors, or non-instance stored property accessors is permitted. Changing
  the body of a stored instance property observing accessor is only permitted
  if the property is not `versioned <versioned entity>`.
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

    This name is intentionally awful to encourage us to come up with a better
    one.

A ``@fixed_contents`` struct is *not* guaranteed to use the same layout as a C
struct with a similar "shape". If such a struct is necessary, it should be
defined in a C header and imported into Swift.

.. note::

    We can add a *different* feature to control layout some day, or something
    equivalent, but this feature should not restrict Swift from doing useful
    things like minimizing member padding.

.. note::

    It would be possible to say that a ``@fixed_contents`` struct only
    guarantees the "shape" of the struct, so to speak, while
    leaving all property accesses to go through function calls. This would
    allow stored properties to change their accessors, or (with the Behaviors
    proposal) to change a behavior's implementation, or change from one
    behavior to another. However, the *most common case* here is probably just
    a simple C-like struct that groups together simple values, with only public
    stored properties and no observing accessors, and having to opt into direct
    access to those properties seems unnecessarily burdensome. The struct is
    being declared ``@fixed_contents`` for a reason, after all: it's been
    discovered that its use is causing performance issues.

    Consequently, as a first pass we may just require all stored properties in
    a ``@fixed_contents`` struct, public or non-public, to have trivial
    accessors, i.e. no observing accessors and no behaviors.

``@fixed_contents`` is a `versioned attribute`. This is so that clients can
deploy against older versions of the library, which may have a different layout
for the struct. (In this case the client must manipulate the struct as if the
``@fixed_contents`` attribute were absent.)


Enums
~~~~~

By default, a library owner may add new cases to a public enum between releases
without breaking binary compatibility. As with structs, this results in a fair
amount of indirection when dealing with enum values, in order to potentially
accommodate new values. More specifically, the following changes are permitted:

- Adding a new case.
- Reordering existing cases, although this is discouraged. In particular, if
  an enum is RawRepresentable, changing the raw representations of cases may
  break existing clients who use them for serialization.
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


Closed Enums
------------

A library owner may opt out of this flexibility by marking a versioned enum as
``@closed``. A "closed" enum may not have any cases with less access than the
enum itself, and may not add new cases in the future. This guarantees to
clients that the enum cases are exhaustive. In particular:

- Adding new cases is not permitted
- Reordering existing cases is not permitted.
- Adding a raw type to an enum that does not have one is still permitted.
- Removing a non-public case is not applicable.
- Adding any other members is still permitted.
- Removing any non-public, non-versioned members is still permitted.
- Adding a new protocol conformance is still permitted.
- Removing conformances to non-public protocols is still permitted.

.. note::

    Were a public "closed" enum allowed to have non-public cases, clients of
    the library would still have to treat the enum as opaque and would still
    have to be able to handle unknown cases in their ``switch`` statements.

``@closed`` is a `versioned attribute`. This is so that clients can deploy
against older versions of the library, which may have non-public cases in the
enum. (In this case the client must manipulate the enum as if the ``@closed``
attribute were absent.) All cases that are not versioned become implicitly
versioned with this number.

Even for default "open" enums, adding new cases should not be done lightly. Any
clients attempting to do an exhaustive switch over all enum cases will likely
not handle new cases well.

.. note::

    One possibility would be a way to map new cases to older ones on older
    clients. This would only be useful for certain kinds of enums, though, and
    adds a lot of additional complexity, all of which would be tied up in
    versions. Our generalized switch patterns probably make it hard to nail
    down the behavior here.


Protocols
~~~~~~~~~

There are very few safe changes to make to protocols:

- A new non-type requirement may be added to a protocol, as long as it has an
  unconstrained default implementation.
- A new optional requirement may be added to an ``@objc`` protocol.
- All members may be reordered, including associated types.

All other changes to the protocol itself are forbidden, including:

- Making an existing requirement optional.
- Making a non-``@objc`` protocol ``@objc`` or vice versa.

Protocol extensions may be more freely modified; `see below`__.

__ #protocol-extensions

.. admonition:: TODO

    It would also be nice to be able to add new associated types with default
    values, but that seems trickier to implement.

Classes
~~~~~~~

Because class instances are always accessed through references, they are very
flexible and can change in many ways between releases. Like structs, classes
support all of the following changes:

- Reordering any existing members, including stored properties.
- Changing existing properties from stored to computed or vice versa.
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
- Adding a new designated initializer, if the class is not publicly
  subclassable.
- Adding a deinitializer.
- Adding new, non-overriding method, subscript, or property.
- Adding a new overriding member, as long as its type does not change.
  Changing the type could be incompatible with existing overrides in subclasses.

Finally, classes allow the following changes that do not apply to structs:

- "Moving" a method, subscript, or property up to its superclass. The
  declaration of the original member must remain along with its original
  availability, but its body may consist of simply calling the new superclass
  implementation.
- A non-final override of a method, subscript, property, or initializer may be
  removed as long as the generic parameters, formal parameters, and return type
  *exactly* match the overridden declaration. Any existing callers should 
  automatically use the superclass implementation.
- ``@noreturn`` may be only added to a method if it is not publicly
  overridable.
- ``@IBOutlet``, ``@IBAction``, and ``@IBInspectable`` may be added to a member
  without providing any extra version information. Removing any of these is
  permitted but discouraged.
- Likewise, ``@IBDesignable`` may be added to a class without providing any
  extra version information. Removing it is permitted but discouraged.
- Changing a class's superclass ``A`` to another class ``B``, *if* class ``B``
  is a subclass of ``A`` *and* class ``B``, along with any superclasses between
  it and class ``A``, were introduced in the latest version of the library.

.. admonition:: TODO

    The latter is very tricky to get right. We've seen it happen a few times in
    Apple's SDKs, but at least one of them, `NSCollectionViewItem`_ becoming a
    subclass of NSViewController instead of the root class NSObject, doesn't
    strictly follow the rules. While NSViewController was introduced in the
    same version of the OS, its superclass, NSResponder, was already present.
    If a client app was deploying to an earlier version of the OS, would
    NSCollectionViewItem be a subclass of NSResponder or not? How would the
    compiler be able to enforce this?

.. _NSCollectionViewItem: https://developer.apple.com/library/mac/documentation/Cocoa/Reference/NSCollectionViewItem_Class/index.html

Other than those detailed above, no other changes to a class or its members
are permitted. In particular:

- New designated initializers may not be added to a publicly-subclassable
  class. This would change the inheritance of convenience initializers, which
  existing subclasses may depend on.
- New ``required`` initializers may not be added to a publicly-subclassable
  class. There is no way to guarantee their presence on existing subclasses.
- ``final`` may not be added to *or* removed from a class or any of its members.
  The presence of ``final`` enables optimization; its absence means there may
  be subclasses/overrides that would be broken by the change.
- ``dynamic`` may not be added to *or* removed from any members. Existing
  clients would not know to invoke the member dynamically.
- A ``final`` override of a member may *not* be removed, even if the type
  matches exactly; existing clients may be performing a direct call to the
  implementation instead of using dynamic dispatch.
- It is not safe to add or remove ``mutating`` or ``nonmutating`` from a member
  or accessor within a class. These modifiers are not `versioned attributes
  <versioned attribute>` and as such there is no safety guarantee for a client
  deploying against an earlier version of the library.
- ``@objc`` and ``@nonobjc`` may not be added to or removed from the class or
  any existing members.
- ``@NSManaged`` may not be added to or removed from any existing members.

.. note:: These restrictions tie in with the ongoing discussions about
  "``final``-by-default" and "non-publicly-subclassable-by-default".

.. admonition:: TODO

    The ``@NSManaged`` attribute as it is in Swift 2 exposes implementation
    details to clients in a bad way. We need to fix this.
    rdar://problem/20829214


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

Non-protocol extensions largely follow the same rules as the types they extend.
The following changes are permitted:

- Adding new extensions and removing empty extensions.
- Moving a member from one extension to another within the same module, as long
  as both extensions have the exact same constraints.
- Moving a member from an extension to the declaration of the base type,
  provided that the declaration is in the same module. The reverse is permitted
  for all members except stored properties, although note that moving all
  initializers out of a type declaration may cause a new one to be implicitly
  synthesized.

Adding, removing, reordering, and modifying members follow the same rules as
the base type; see the sections on structs, enums, and classes above.


Protocol Extensions
-------------------

Protocol extensions follow slightly different rules from other extensions; the
following changes are permitted:

- Adding new extensions and removing empty extensions.
- Moving a member from one extension to another within the same module, as long
  as both extensions have the exact same constraints.
- Adding any new member.
- Reordering members.
- Removing any non-public, non-versioned member.
- Changing the body of any methods, initializers, or accessors.


Operators
~~~~~~~~~

Operator declarations are entirely compile-time constructs, so changing them
does not have any affect on binary compatibility. However, they do affect
*source* compatibility, so it is recommended that existing operators are not
changed at all except for the following:

- Making a non-associative operator left- or right-associative.

Operator declarations are not versioned.


Typealiases
~~~~~~~~~~~

Typealiases only exist at compile-time, so changing a top-level typealias
affects source compatibility but not binary compatibility. Since a typealias is
only made up of its name and its type, it is recommended that a public
top-level typealias is never changed at all after being published.

Public typealiases within structs, enums, and protocols may be used for
protocol conformances (to satisfy associated type requirements), not only
within the library but within client modules as well. Therefore, changing a
member typealias in any way is not permitted.

It is always permitted to change the *use* of a public typealias to its
underlying type, and vice versa, at any location in the program.

Neither top-level nor member typealiases are versioned.


A Unifying Theme
~~~~~~~~~~~~~~~~

So far this document has talked about ways to give up flexibility for several
different kinds of declarations: ``@inlineable`` for functions,
``@fixed_contents`` for structs, etc. Each of these has a different set of
constraints it enforces on the library author and promises it makes to clients.
However, they all follow a common theme of giving up the flexibility of future
changes in exchange for improved performance and perhaps some semantic
guarantees. Therefore, all of these attributes are informally referred to as
"fragility attributes".

Given that these attributes share several characteristics, we could consider
converging on a single common attribute, say ``@fixed``, ``@inline``, or
``@fragile``. However, this may be problematic if the same declaration has
multiple kinds of flexibility, as in the description of classes above.


Versioning Internal Declarations
================================

The initial discussion on versioning focused on ``public`` APIs, making sure
that a client knows what features they can use when a specific version of a
library is present. Inlineable functions have much the same constraints, except
the inlineable function is the client and the entities being used may not be
``public``.

Adding a versioning annotation to an ``internal`` entity promises that the
entity will be available at link time in the containing module's binary. This
makes it safe to refer to such an entity from an inlineable function. If the
entity is ever made ``public``, its availability should not be changed; not
only is it safe for new clients to rely on it, but *existing* clients require
its presence as well.

.. note::

    Why isn't this a special form of ``public``? Because we don't want it to
    imply everything that ``public`` does, such as requiring overrides to be
    ``public``.

Because a versioned class member may eventually be made ``public``, it must be
assumed that new overrides may eventually appear from outside the module unless
the member is marked ``final`` or the class is not publicly subclassable.

Non-public conformances are never considered versioned, even if both the
conforming type and the protocol are versioned.

Entities declared ``private`` may not be versioned; the mangled name of such an
entity includes an identifier based on the containing file, which means moving
the declaration to another file changes the entity's mangled name. This implies
that a client would not be able to find the entity at run time if the source
code is reorganized, which is unacceptable.

.. note::

    There are ways around this limitation, the most simple being that versioned
    ``private`` entities are subject to the same cross-file redeclaration rules
    as ``internal`` entities. However, this is a purely additive feature, so to
    keep things simple we'll stick with the basics.

We could do away with the entire feature if we restricted inlineable functions
and fixed-contents structs to only refer to public entities. However, this
removes one of the primary reasons to make something inlineable: to allow
efficient access to a type while still protecting its invariants.


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
present for code executing within the context. This allows the compiler to
answer the question, "Given what I know about where this code will be executed,
what can I assume about a particular entity being used?".

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


Inlineable Code
~~~~~~~~~~~~~~~

By default, the availability context for a library always includes the latest
version of the library itself, since that code is always distributed as a unit.
However, this is not true for functions that have been marked inlineable (see
`Inlineable Functions`_ above). Inlineable code must be treated as if it is
outside the current module, since once it's inlined it will be.

For inlineable code, the availability context is exactly the same as the
equivalent non-inlineable code except that the assumed version of the
containing library is the version attached to the ``@inlineable`` attribute.
Code within this context must be treated as if the containing library were just
a normal dependency.

A versioned inlineable function still has an exported symbol in the library
binary, which may be used when the function is referenced from a client rather
than called. This version of the function is not subject to the same
restrictions as the version that may be inlined, and so it may be desirable to
compile a function twice: once for inlining, once for maximum performance.


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

- ``trivial``: Promises that the type is `trivial`.

- ``size_in_bits(N)``: Promises that the type is not larger than a certain
  size. (It may be smaller.)

- ``fixed_size``: Promises that the type has *some* size known at compile-time,
  allowing optimizations like promoting allocations to the stack. Only applies
  to fixed-contents structs and closed enums, which can already infer this
  information; the explicit annotation allows it to be enforced.

Collectively these features are known as "performance assertions", to
underscore the fact that they do not affect how a type is used at the source
level, but do allow for additional optimizations. We may also expose some of
these qualities to static or dynamic queries for performance-sensitive code.

.. note:: Previous revisions of this document contained a ``no_payload``
    assertion for enums. However, this doesn't actually offer any additional
    optimization opportunities over combining ``trivial`` with ``size_in_bits``,
    and the latter is more flexible.

All of these features need to be versioned, just like the more semantic
fragility attributes above. The exact spelling is not proposed by this document.


Resilience Domains
==================

As described in the `Introduction`_, the features and considerations discussed
in this document do not apply to libraries distributed in a bundle with their
clients. In this case, a client can rely on all the current implementation
details of its libraries when compiling, since the same version of the library
is guaranteed to be present at runtime. This allows more optimization than
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
assume that version of the library will be present at runtime.


Checking Binary Compatibility
=============================

With this many manual controls, it's important that library owners be able to
check their work. Therefore, we intend to ship a tool that can compare two
versions of a library's public interface, and present any suspect differences
for verification. Important cases include but are not limited to:

- Removal of versioned entities.

- Incompatible modifications to versioned entities, such as added protocol
  conformances lacking versioning information.

- Unsafely-backdated "fragile" attributes as discussed in the `Giving Up
  Flexibility`_ section.

- Unsafe modifications to entities marked with the "fragile" attributes, such as
  adding a stored property to a ``@fixed_contents`` struct.


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


Summary
=======

When possible, Swift gives library developers freedom to evolve their code
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

- `SE-0030 Property Behaviors`_
- (planned) Overridable methods in extensions
- (planned) Making classes "sealed" by default
- (planned) Restricting retroactive modeling (protocol conformances for types you don't own)
- (planned) Default implementations in protocols
- (planned) Open and closed enums
- (planned) Syntax for declaring "versioned" entities and their features
- (planned) Syntax for declaring inlineable code
- (planned) Syntax for declaring fixed-contents structs
- (future) Performance annotations for types
- (future) Attributes for stored property accessors

.. _SE-0030 Property Behaviors: https://github.com/apple/swift-evolution/blob/master/proposals/0030-property-behavior-decls.md

This does not mean all of these proposals need to be accepted, only that their
acceptance or rejection will affect this document.


Glossary
========

.. glossary::

  ABI
    The runtime contract for using a particular API (or for an entire library),
    including things like symbol names, calling conventions, and type layout
    information. Stands for "Application Binary Interface".

  API
    An `entity` in a library that a `client` may use, or the collection of all
    such entities in a library. (If contrasting with `SPI`, only those entities
    that are available to arbitrary clients.) Marked ``public`` in
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
    term "entities" also includes conformances, since these have a runtime
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

  trivial
    A value whose assignment just requires a fixed-size bit-for-bit copy
    without any indirection or reference-counting operations.

  versioned entity
    See `Publishing Versioned API`_.

  versioned attribute
    See `Publishing Versioned API`_.
