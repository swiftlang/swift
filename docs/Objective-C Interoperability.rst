============================
Objective-C Interoperability
============================

This document tracks the differences between the Swift and Objective-C ABIs and
class models, and what it would take to merge the two as much as possible. The
format of each section lays out the differences between Swift and Objective-C,
then describes what needs to happen for a user to mix the two seamlessly.

Major design question:

- Are all Swift objects ``id``-compatible? That is, can any random ``id`` refer
  to a Swift object? (Forget retain and release for now.)

  The other possibility is that only Swift objects that inherit from Objective-C
  objects are ``id``-compatible. Any other Swift object is treated as some
  opaque type by the compiler. (A further decision: can you send messages to
  these things if they have *known* type?)


Use Cases
=========

*Unfinished and undetailed.*

Simple Application Writer
-------------------------

I want to write my new iOS application in Swift, using all the Objective-C
frameworks that come with iOS.

Requirements:

- Can send messages to Objective-C objects from Swift.
- Can subclass Objective-C classes, override Objective-C methods, and
  implement Objective-C protocols.
- Can use my subclass as an Objective-C object (e.g. a delegate).


Intermediate Application Writer
-------------------------------

I want to write my new application in Objective-C, but there's a really nice
Swift framework I want to use.

Requirements:

- Can subclass (at least some) Swift objects in Objective-C.
- Can call (at least some) Swift methods from Objective-C.


New Framework Writer
--------------------

I want to write a framework that can be used by anyone.

Requirements:

- Can call (at least some) Swift methods from Objective-C.


Intermediate Framework Writer
-----------------------------

I have an existing Objective-C framework that I want to move to Swift.

Requirements:

- Can subclass Objective-C classes in Swift.
- Can call (at least some) Swift methods from Objective-C.

Decisions:

- Should I expose Swift entry points as API?
- If so, should they be essentially the same as the Objective-C entry points, or
  should I have a very different interface that's more suited for Swift (and
  easily could be "better")?


End User
--------

- Things should be fast.
- Things should not take a ton of memory.


Nice to Have (uncategorized)
----------------------------

- Can write a Swift extension for an Objective-C class.
- Can write a Swift extension for an Objective-C class that adopts an
  Objective-C protocol.
- Can write a Swift extension for an Objective-C class that exposes arbitrary
  new methods in Objective-C.


Level 1: Message-passing
========================

*Assuming an object is known to be a Swift object or an Objective-C object at
compile-time, what does it take to send a message from one to the other?*


ARC
---

  By default, objects are passed to and returned from Objective-C methods as +0
  (i.e. non-owned objects). The caller does not have to do anything to release
  returned objects, though if they wish to retain them they may be able to steal
  them out of the top autorelease pool. (In practice, the caller *does* retain
  the arguments for the duration of the method anyway, unless it can be proven
  that nothing interferes with the lifetime of the object between the load and
  the call.)

  Objective-C methods from certain method families do return +1 objects, as do
  methods explicitly annotated with the ``ns_returns_retained`` attribute.

  All Swift class objects (i.e. as opposed to structs) are returned as +1 (i.e.
  owned objects). The caller is responsible for releasing them.

Swift methods that are exposed as Objective-C methods will have a wrapper
function (thunk) that is responsible for retaining all (object) arguments and
autoreleasing the return value.

**Not yet decided**: do we need to expose *both* entry points here, or only the
Objective-C one?

*Swift methods will **not** be exposed as* ``ns_returns_retained`` because they
should behave like Objective-C methods when called through an* ``id``.


Arguments
---------

  Objective-C currently requires that the first argument be ``self`` and the
  second be ``_cmd``. The explicit arguments to a method come after ``_cmd``.
  
  Swift only requires that the first argument be ``self``. The explicit
  arguments come after ``self``.

The thunk mentioned above can shift all arguments over...which doesn't really
cost anything extra since we already have to retain all the arguments.


Output Parameters
-----------------

  Because Objective-C does not have tuples, returning multiple values is
  accomplished through the use of pointer-to-object-pointer parameters, such as
  ``NSError **``. Additionally, objects returned through these parameters are
  conventionally autoreleased, though ARC allows this to be specified
  explicitly.

  Swift has tuples and does not have pointers, so the natural way to return
  multiple values is to return a tuple. The retain-count issue is different
  here: with ARC, the tuple owns the objects in it, and the caller owns the
  tuple.

  Swift currently also has ``[byref]`` arguments. Whether or not these will be
  exposed to users and/or used for Objective-C out parameters is still
  undecided.

*This issue has not been resolved, but it only affects certain API.*


Messaging ``nil``
-----------------

  In Objective-C, the result of messaging ``nil`` is defined to be a zero-filled
  value of the return type. For methods that return an object, the return value
  is also ``nil``. Methods that return non-POD C++ objects attempt to
  default-construct the object if the receiver is ``nil``.

  In Swift, messaging ``nil`` is undefined, and hoped to be defined away by the
  type system through liberal use of some ``Optional`` type.

  - I've seen other languages explicitly request the Objective-C behavior using
    ``foo.?bar()``, though that's not the prettiest syntax in the world.
    -Jordan

As long as the implementation of ``Optional`` is layout-compatible with an
object pointer, and an absent ``Optional`` is represented with a null pointer,
this will Just Work™.


Overloading
-----------
  In Objective-C, methods cannot be overloaded.

  In Swift, methods can have the exact same name but take arguments of different
  types.

  Note that in Swift, all parameters after the first are part of the method
  name, unless using the "selector syntax" for defining methods::

    // 1. foo:baz:
    func foo(Int bar, Int baz);
  
    // 2. foo:qux:
    func foo(Int bar, Int qux);
  
    // 3. foo:qux: (same as above)
    func foo(Int bar) qux(Int quux);
  
    // 4. foo:baz: (but different type!)
    func foo(Int bar, Char baz);
  
    a.foo(1, 2)      // ambiguous in Swift (#1 or #2?)
    a.foo(1, baz=2)  // calls #1
    a.foo(1, qux=2)  // calls #2/3 (the same method)
    a.foo(1, 'C')    // calls #4, not ambiguous in Swift!
  
    [a foo:1 baz:2]; // ambiguous in Objective-C (#1 or #4?)
    [a foo:1 qux:2]; // calls #2/3 (the same method)

The Swift compiler should not let both #1 and #4 be exported to Objective-C.
It should already warn about the ambiguity between #1 and #2 without using
named parameters.


Level 2: Messaging ``id``
=========================

*If a Swift object can be referenced with* ``id``, *how do you send messages to*
*it?*

Note: the answer might be "Swift objects can't generally be referenced with
``id``".


``isa`` Pointers
----------------
  The first word of every Objective-C object is a pointer to its class.
  
  We might want to use a more compact representation for Swift objects...

...but we can't; see below.


Method Lookup
-------------
  Objective-C performs method lookup by searching a sequence of maps for a
  given key, called a *selector*. Selectors are pointer-sized and uniqued
  across an entire process, so dynamically-loaded methods with the same name as
  an existing method will have an identical selector. Each map in the sequence
  refers to the set of methods added by a category (or the original class). If
  the lookup fails, the search is repeated for the superclass.

  Swift performs method lookup by vtable. In order to make these vtables
  non-fragile, the offset into a vtable for a given message is stored as a
  global variable. Rather than chaining searches through different message
  lists to account for inheritance and categories, the container for each
  method is known at compile-time. So the final lookup for a given method looks
  something like this::

    vtable[SUBCLASS_OFFSET + METHOD_OFFSET]

Swift class objects will have ``isa`` pointers, and those ``isa`` pointers will
have an Objective-C method list at the very least, and probably a method cache
as well. The methods in this list will refer to the Objective-C-compatible
wrappers around Swift methods described above.

The other words in the ``isa`` structure may not be used in the same way as they
are in Objective-C; only ``objc_msgSend`` has to avoid special-casing Swift
objects. Most of the other runtime functions can probably do a check to see if
they are dealing with a Swift class, and if so fail nicely.


Level 3a: Adopting Objective-C Protocols in Swift
=================================================

*To be written.*

- Basically necessary for implementing an AppKit app in Swift.


Level 3b: Adopting Swift Protocols in Objective-C
=================================================

*To be written.*

- Requires generating Swift protocol vtables.

*Note: including protocol implementations is essentially the same as implicitly*
*adding a category.*


Level 4a: Subclassing Objective-C Classes in Swift
==================================================

*To be written.*

- Requires generating Objective-C-compatible method lists.
- When a new method is marked as API, does it automatically get the Objective-C
  calling conventions by default?


Level 4b: Subclassing Swift Classes in Objective-C
==================================================

*To be written.*

- May require generating Swift vtables.

  Alternative: if a method is exposed for overriding, it only gets an
  Objective-C entry point. (Downsides: performance, other platforms will hate
  us.)

  Alternative: only Swift classes with an Objective-C class in their hierarchy
  can be subclassed in Objective-C. Any overridden methods must be exposed as
  Objective-C already. (Downsides: framework authors could forget to inherit
  from NSObject, Swift code is penalized ahead of time.)

  Alternative: only Swift classes with an Objective-C class in their hierarchy
  are *visible* in Objective-C. All other Swift objects are opaque.
  (Downsides: same as above.)


Level 5a: Adding Extensions to Objective-C Classes in Swift
===========================================================

*To be written.*

- May require generating Objective-C-compatible method lists.
- Less clear what the *default* calling convention should be for new methods.


Level 5b: Adding Categories to Swift Classes in Objective-C
===========================================================

*To be written.*

- Does not actually *require* generating Swift vtables. But we could if we
  wanted to expose Swift entry points for these methods as well.

- Does require an Objective-C-compatible ``isa`` to attach the new method list
  to.


Level 6: Dynamic Subclassing
============================

*To be written, but probably not an issue...it's mostly the same as statically*
*subclassing, right?*


Level 7: Method Swizzling
=========================

I'm okay with just saying "no" to this one.

