=================
 Weak References
=================

:Author: John McCall
:Date: 2013-05-23

**Abstract:** This paper discusses the general concept of weak
references, including various designs in other languages, and proposes
a new core language feature and a more sophisticated runtime support
feature which can be exploited in the standard library.

Reference Graphs
================

Let's run through some basic terminology.

Program memory may be seen abstractly as a (not necessarily connected)
directed graph where the nodes are objects (treating transient
allocations like stack frames as objects) and the edges are
references.  (Multi-edges and self-edges are permitted, of course.)

We may assign a level of strength to each of these edges.  We will
call the highest level *strong*; all others are some flavor of *weak*.

Every object has a *strength of reference* which arises from the
reference graph.  This can be any level of strength or the special
value *unreferenced*.  The strength of a path is the minimum strength
of any edge in the path.  The strength of a set of paths is the
maximum strength of all the paths, unless the set is empty, in which
case it is *unreferenced*.

In general, the implementation is only outright forbidden to
deallocate an object if it is strongly referenced.  However,
being somehow weakly referenced may trigger some sort of additional
guarantee; see the Language Precedents section.

In a cycle-collecting environment, certain nodes are given special
treatment as *roots*; these nodes are always strongly referenced.
Otherwise, the strength of reference for an object is the strength
of all paths from any root to the object.

In a non-cycle-collecting environment, the strength of reference for
an object is the strength of all the direct references to that
object, taken as length=1 paths.  Note that this environmental
consideration becomes a language guarantee: even if the implementation
can trivially prove that an object is referenced only by itself, it
is still not permitted to deallocate the object.

It is common for certain kinds of reference to not receive a full
guarantee.  For example, a strong reference from a local variable
may lose effectiveness as soon as the variable is no longer needed
(but before it formally leaves scope).  This is pervasive in GC
environments but also true in e.g. ObjC ARC.

Language and Library Precedents
===============================

We're only going to discuss *managed* precedents here.

It is possible to create a kind of weak reference by just not
providing any special behavior to an object reference; if the
object is deallocated, the reference will dangle and uses of it
will likely crash or cause corruption.  This could happen by
e.g. declining to insert reference-count manipulations for a
particular variable (in an ARC-like environment) or not mapping
a variable's frame offset as a pointer in a type map (in a
precise-GC environment).  We will call this *dangling weak*.

Objective-C
-----------

All modes of Objective-C automate memory management for
synthesized properties.  In GC and ARC, accessors just
use the normal semantics for the underlying ivar (plus
copy/atomic guarantees, of course).  In MRC, accessors
use dangling weak semantics unless they're :code:`retain`
or :code:`copy`, in which case they maintain a +1 refcount
invariant on the referent.

In GC and ARC, variables qualified with :code:`__weak` are
immediately zeroed out when the referenced object begins
deallocation.  There is no syntactic difference on use;
it's just possible that the value read will be :code:`nil`
instead of whatever was last written there.  There is an
opt-in warning in ARC for certain uses of values loaded
from :code:`__weak` ivars.

In GC, it is also possible to construct a dangling
weak reference by storing an object pointer into (1) unscanned
heap memory or (2) an instance variable that's not of
object-pointer type and isn't qualified with :code:`__strong`
or :code:`__weak`.  Otherwise, object references are strong
(including all references on the stack).

In ARC, it is possible to construct a dangling weak reference
by using the :code:`__unsafe_unretained` qualifier or by
bridging a pointer value to a C pointer type.

C++
---

C++ smart pointers (e.g. :code:`std::unique_ptr`) typically
permit the creation of a dangling-weak reference by
providing an accessor to get the pointer as a normal C
pointer.  (Even if they didn't have :code:`get()`, you could
manually call :code:`operator->` to get the same effect.)

C++'s :code:`std::shared_ptr` permits the formation of
weak pointers (:code:`std::weak_ptr`) from shared pointers.
It is not possibly to directly use a weak pointer;  it must
first be converted back to a :code:`shared_ptr`, either by
using the :code:`lock()` operation (which produces a null
pointer if the referent has been deallocated) or by directly
constructing a :code:`shared_ptr` with the :code:`weak_ptr`
(which throws an exception if the referent has been deallocated).
There is also a way to explicitly query whether a
:code:`weak_ptr` is still valid, which may be more efficient
than checking the result of the cast.

Java
----

Java does not provide any facility for dangling weak references.
The standard library does provide three levels of weak reference
(in :code:`java.lang.ref`).  References cannot be re-seated
(although they can be explicitly cleared), and users must call
:code:`get()` in order to access the value, which may yield
:code:`null`.

There is a great deal of interesting discussion of these
reference classes `here <http://www.kdgregory.com/index.php?page=java.refobj>`.

Java :code:`Reference` objects may be constructed with an
optional :code:`ReferenceQueue`;  if so, then when the
object's reachability changes, the reference object will be
added to that queue.  This permits data structures to clean
up after cleared soft references without needing to either
periodically scan the entire structure or be fully lazy.
Additional data may be added to the reference object by
subclassing it.

The references are presented in order of decreasing strength.

:code:`SoftReference` is a sort of quasi-strong reference
which holds onto the object until the VM begins to run out
of memory.  Soft references to softly-referenced objects are
guaranteed to have been cleared before the VM can throw an
:code:`OutOfMemoryError`.  The reference will be cleared
before it is added to its reference queue (and so the
reference queue cannot resurrect the object).  The intent
of soft references is to enable memory-sensitive caches,
but in practice a memory-sensitive cache would probably
want to implement a more subtle replacement strategy than
"drop things at random as soon as memory runs low".  A
more interesting use is a memory-guided circuit-breaker:
when building up a very large structure, hold it in a
soft reference, and if that references goes null during
construction, just bail out.  But that's a pretty tricky
use-case to get right.

:code:`WeakReference` is intended for use in non-memory-sensitive
weak caches, like a uniquing cache;  it persists only as long
as the referent is more strongly referenced.  The reference
will be cleared before it is added to its reference queue (and
so the reference queue cannot resurrect the object).

:code:`PhantomReference` provides a way to attach extra
finalization to an object without actually using finalizers
(which have several problems, including the ability to
resurrect the object).  The phantom reference *always*
presents :code:`null` as its value and is therefore itself
useless as a reference.  Phantom references are enqueued
after the object is finalized and therefore at a point when
there can be no references to the object within the VM
at all.  However, the object itself cannot be deallocated
until the phantom references are all cleared or themselves
deallocated, which I believe is for the convenience of native
code that may hold a dangling weak reference to the referent
(or which may be able to directly read the reference).

.NET
----

The :code:`WeakReference` class in .NET is similar to
Java's :code:`WeakReference` class in that the value
cannot be accessed directly;  it must be accessed
via the :code:`Target` property, which may yield
:code:`null`.  The reference may be reseated to a
different value.

Weak references may be created *long*, which permits the
target object to be finalized but not actually deallocated.

Python
------

A :code:`weakref` acts like a function object; it is created
with a particular value, which cannot be reseated.  The
function will yield :code:`None` if the referent is collected.

There is library functionality to automatically proxy a value
as a weak reference.  An exception is thrown if an operation
is performed on the proxy but the referent has been collected.

A :code:`weakref` may be constructed with a callback function.
The callback will be called after the weak reference is cleared;
it is, however, passed the weak ref object itself.

Ruby
----

A :code:`WeakRef` is automatically a proxy for an object.
There is a :code:`weakref_alive` method to query whether the
reference is still alive; another other operation will cause
an exception to be thrown.

Rust
----

As far as I can tell, there is nothing like a weak reference
in Rust at the moment.

A *managed pointer* (:code:`@int`) is a strong reference
subject to GC.

An *owning pointer* (:code:`~int`) is a strong reference
that cannot be cloned (copying the pointer actually copies the
underlying data).

A *borrowed pointer* (:code:`&int`) is essentially a dangling
weak reference that is subject to static restrictions which
ensure that it doesn't actually dangle.  It is thus primarily
a performance optimization.

A *raw pointer* (:code:`*int`) is a dangling weak reference.

Haskell
-------

Yes, of course Haskell has weak references.

A :code:`Weak t` is an association between a hidden key
and a visible value of type :code:`t`.
:code:`doRefWeak theRef` is an :code:`IO (Maybe t)`.

A weak reference may be constructed with an optional
:code:`IO ()` which will be run when the referent is
collected.  This finalizer may (somehow) refer to the key
and value without itself keeping them alive;  it is also
explicitly permitted to resurrect them.


Use Cases
=========

There are many problems that are potentially addressable with
functionality like weak references.  It is not at all obvious
that they should be addressed with the same language feature.

Back references
---------------

Given that Swift is not cycle-collecting, far and away the
most important use case is that of the *back-reference*:
a reference *R* to an object which holds a strong reference
(possibly indirectly) to the object holding *R*.  Examples
include:

- A 'previousNode' pointer in a doubly-linked list.

- A 'parent' pointer in a render tree.

- An edge in a general graph structure.

These have several properties in common:

- Using strong references will require a lot of explicit
  code to tear down the reference cycles.

- In practice, it is very unlikely that the reference will be
  traversed after the referent is deallocated, and the
  programmer is likely to see this as a potential bug.

- These references may be accessed very frequently, so
  performance is important.

Caches
------

Weak caches are used in order to prevent a cache from taking
over all available memory.  By being tied to the reachability
of a value, the cache prevents entries from spuriously
expiring when their values are still in active use;  but by
using weak references, the cache permits the system to
deallocate values that are no longer in use.

Generally, a data structure using weak references extensively
also needs some way to receive notification that the weak
reference was collected.  This is because entries in the data
structure are likely to have significant overhead even if the
value is collected.  A weak data structure which receives no
notification that a reference has been invalidated must either
allow these entries to accumulate indefinitely or must
periodically scan the entire structure looking for stale entries.

A weak reference which permits immediate deallocation of its
referent when the last strong reference is dropped is
substantially less useful for the implementation of a weak
cache.  It is a common access pattern (for, say, a memoizing
cache) for a value to be looked up many times in rapid
succession, but for each use to be temporarlly disjoint
from the others.  A naive use of weak references in this case
will simply cause the cache to thrash.  This problem is less
likely to arise in an environment with nondeterministic
collection because the entry is likely to service multiple
lookups between collections.

It is likely that users implementing weak data structures
would prefer a highly flexible infrastructure centered around
resurrection and notifications of reaching a zero refcount
than a more rigid system built directly into the language.
Since the Swift model is built around statically-inserted
operations rather than a memory scanner, this is much more
workable.

External Finalization
---------------------

Finalization models built around calling a method on the
finalized object (such as Objective-C's :code:`-dealloc`)
suffer from a number of limitations and problems:

  - Since the method receives a pointer to the object being
    deallocated, the implementation must guard against
    attempts to resurrect the object.  This may complicate
    and/or slow down the system's basic reference-management
    logic, which tends to be quite important for performance.

  - Since the method receives a pointer to the object being
    deallocated, the implementation must leave the object at
    least a minimally valid state until the user code is
    complete.  For example, the instance variables of a
    subclass cannot be destroyed until a later phase of
    destruction, because a superclass finalizer might invoke
    subclass behavior.  (This assumes that the dynamic type
    of the object does not change during destruction, which
    is an alternative that brings its own problems.)

  - Finalization code must be inherent to the object; other
    objects cannot request that code be run when the object
    is deallocated.  For example, an object that registers
    itself to observe a certain event source must explicitly
    deregister itself in a finalizer; the event source cannot
    simply automatically drop the object when it is
    deallocated.

Proposal
========

Looking at these use-cases, there are two main thrusts:

  - There is a pervasive need for an ability to set up a
    back-reference to an object.  These references must be
    designed for convenient use by non-expert users.

  - There are a number of more sophisticated use cases which
    would be well-served by a general library facility for
    receiving notifications before an object is finalized,
    preferably with some ability to interrupt that process.
    Uses of this facility will generally be encapsulated
    within higher-level abstractions like a weak cache,
    and so convenience of use is not a major priority.

Therefore, I propose adding a :code:`[backref]` attribute to
accomplish the former and a core runtime facility that is
capable of accomplishing the latter.  We may then add
standard-library types designed to exploit the runtime
facility.

:code:`[backref]`
-----------------

In the user model, :code:`backref` is an attribute which
may be applied to any :code:`var` declaration of
reference type.  For type-system purposes, the variable
behaves exactly like a normal variable of that type, except:

  - it does not maintain a +1 reference count invariant and

  - loading from the variable after the referent has started
    destruction causes an assertion failure.

Asserting and Uncheckable
.........................

There should not be a way to check whether a :code:`backref`
is still valid.

- An invalid back-reference is a consistency error that
  we should encourage programmers to fix rather than work
  around by spot-testing for validity.

- Contrariwise, a weak reference that might reasonably be
  invalidated during active use should be checked for validity
  at *every* use.  We can provide a simple library facility
  for this pattern.

- Permitting implicit operations like loads to fail in a
  recoverable way may end up complicating the language model
  for error-handling.

- By disallowing recovery, we create a model where the only
  need to actually register the weak reference with the system
  is to enable a consistency check.  Users who are confident
  in the correctness of their program may therefore simply
  disable the consistency check without affecting the semantics
  of the program.  In this case, that leaves the variable a
  simple dangling-weak reference.

Implementation
..............

The standard implementation for a weak reference requires the
address of the reference to be registered with the system so
that it can be cleared when the referent is finalized.  This
has two problems:

- It forces the runtime to maintain a side-table mapping
  objects to the list of weak references; generally this
  adds an allocation per weakly-referenced object.

- It forces the representation of weak references to either
  be non-address-invariant or to introduce an extra level of
  indirection.

For some use cases, this may be warranted; for example, in
a weak cache it might come out in the noise.  But for a simple
back-reference, these are substantial penalties.

Dave Z. has proposed instead using a weak refcount, analogous to a
strong refcount.  Ownership of a weak retain can be easily transferred
between locations, and it does not require a side-table of an object's
weak references.  However, it does have a very important downside:
since the system cannot clear all the references, it is impossible to
actually deallocate an object that is still weakly-referenced
(although it can be finalized).  Instead, the system must wait for
all the weak references to at least be accessed.

This downside could be very problematic for a general weak reference.
However, it's fine for a back-reference, which will usually be
short-lived after its referent is finalized.

Declaration Attribute or Type Attribute
.......................................

I have described this as a declaration attribute rather than
a type attribute.  This prevents the attribute from appearing
at nested locations in the type-system; for example, it cannot
appear on a tuple element, a function result type, or a
generic argument.

The reason for this is primarily simplification.  The special
properties of back-references are tied to l-values, not r-values.
For example, the result of loading out of a back-reference is
an ordinary strong reference, and there's no particular reason
to allow function arguments or results to be back-references
(although it's not insensible, either).  If we raised this into
the type-system, there would need to be a certain amount of
extra complexity around :code:`backref`, somewhat analogous to
the complexity entailed by atomic types in Clang.

On the other hand, it is problematic in a number of ways
to have a major variation in semantics that is not reflected in
the type system:

- It forces users to use awkward workarounds if they want to
  make, say, arrays of back-references.

- It makes back-references less composable by, say, preventing
  them from being stored in a tuple.

- It forces SIL and IR-gen to recognize a major difference in
  implementation piece-meal.

- It automatically enables certain things (like passing the
  address of a :code:`[backref] T` variable to a
  :code:`[byref] T` parameter) that perhaps ought to be
  more carefully considered.

I tend to think that :code:`backref` is one of a class of
similar qualifier-like features that we may find ourselves
wanting to support in the type system, but I can't really
speak to the cost of introducing and maintaining that support.

:code:`backref`-Capable Types
.............................

Swift reference types can naturally be made to support any kind of
semantics, and I'm taking it on faith that we could enhance ObjC
objects to support whatever extended semantics we want.  There
are, however, certain Swift value types which have reference-like
semantics that it could be useful to extend :code:`backref` to:

- Being able to conveniently form an optional back-reference seems
  like a core requirement.  However, given :code:`backref` as a
  type attribute, this could be stated as an
  :code:`Optional<[backref] T>` rather than a
  :code:`[backref] (Optional<T>)`.

- Being able to form a back-reference to a slice or a string seems
  substantially less important.

While it would be straightforward to add a magic protocol used
to desugar :code:`[backref] T` into an associated implementation
type with protocol members for loading, initializing, and assigning,
I think we can avoid designing this (especially for 1.0) provided
we have convenient optional back-references.


Generic Weak Support
--------------------

All other uses for weak references can be glossed as desiring
some amount of additional work to occur when the strong reference
count for an object reaches zero.  This necessarily entails a
global side-table of such operations, but I believe that's
acceptable as long as it's relegated to less common use-cases.

It is important that the notification mechanism not require
executing code re-entrantly during the finalization process.

I suggest adopting an interface centered around the Java
concept of a :code:`ReferenceQueue`.  A reference structure
is registered with the runtime for a particular object with
a particular set of flags and an optional reference queue:

::
  struct Reference {
    void *Referent; // must be non-null upon registration
    struct ReferenceQueue *Queue; // must be valid or null
    size_t Reserved[2];
  };

  void swift_registerReference(struct Reference *reference,
                               size_t flags);

The user/library code is responsible for allocating these structures
and initializing the first two fields, and it may include arbitrary
fields before or after the :code:`Reference` section, but while the
reference is registered with the runtime, the entire :code:`Reference`
section becomes reserved and user/library code must not access it in
any way.

The flags include:

- A priority.  Should be constrained to two or three bits.  References
  are processed in order of decreasing priority; as long as a
  reference still exists with higher priority, references with lower
  priority cannot be processed.  Furthermore, as long as any reference
  exists, the referent cannot be finalized.

- Whether to automatically clear the reference when processing it.
  Note that a cleared reference is still considered to be
  registered with the runtime.

These could be combined so that e.g. even priorities cause
an automatic clear and odd priorities do not;  this would avoid some
odd effects.

The runtime may assume that explicit user operations on the same
reference will not race with each other.  However, user operations on
different references to the same referent may be concurrent, either
with each other or with other refcount operations on the referent.

The operations on references are as follows:

::
  void *swift_readReference(struct Reference *reference);

This operation atomically either produces a strong reference to the
referent of the given object or yields :code:`null` if the referent
has been finalized (or if the referent is :code:`null`).  The
reference must currently be registered with the runtime.

::
  void swift_writeReference(struct Reference *reference,
                            void *newReferent);

This operation changes the referent of the reference to a new object,
potentially :code:`null`.  The argument is taken at +0.  The reference
must currently be registered with the runtime.  The reference keeps
the same flags and reference queue.

::
  void swift_unregisterReference(struct Reference *Reference);

This operation clears a reference, removes it from its reference
queue (if it is enqueued), and unregisters it from the runtime.
The reference must currently be registered with the runtime.

I propose the following simple interface to a ReferenceQueue;
arguably, however, it ought to be a reference-counted library
type with a small amount of native implementation:

::
    struct ReferenceQueue;
    struct ReferenceQueue *swift_createReferenceQueue(void);

Allocate a new reference queue.

::
    void swift_destroyReferenceQueue(struct ReferenceQueue *queue);

Destroy a reference queue.  There must not be any references with
this queue currently registered with the runtime.

::
     struct Reference *swift_pollReferenceQueue(struct ReferenceQueue *queue);

Library Directions
------------------

The library should definitely provide the following type:

- :code:`WeakReference<T>`: a value type which maintains a reference
  that is automatically cleared when its referent is finalized.
  It provides operations to set its referent and to get its referent
  as an :code:`Optional<T>`.

The library should consider providing the following types:

- A simple, memory-sensitive weak cache.

- :code:`Finalizer`: a value type which is constructed with a referent
  and a :code:`() -> ()` function and which causes the function to be
  run (on a well-known dispatch queue?) when the object is finalized.

