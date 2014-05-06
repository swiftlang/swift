:orphan:

The Swift Array Design
======================

:Author: Dave Abrahams
:Date: 2014-04-10

.. raw:: html

   <style>
   tt {
     background-color: #f2f2f2;
   }
   div.content ul > li {
    background: none;
    padding: 0 0 0 0.5em;
    list-style-image: none;
    list-style-type: disc;
    }
   </style>

Goals
-----

1. Performance equivalent to C arrays for subscript get/set of
   non-class element types is the most important performance goal.

2. It should be possible to receive an ``NSArray`` from Cocoa,
   represent it as an ``Array<AnyObject>``, and pass it right back to
   Cocoa as an ``NSArray`` in O(1) and with no memory allocations.
      
3. Arrays should be usable as stacks, so we want amortized O(1) append
   and O(1) popBack.  Together with goal #1, this implies a
   ``std::vector``\ -like layout, with a reserved tail memory capacity
   that can exceed the number of actual stored elements.

To achieve goals 1 and 2 together, we use static knowledge of the
element type: when it is statically known that the element type is not
a class, code and checks accounting for the possibility of wrapping an
``NSArray`` are eliminated.  An ``Array`` of Swift value types always
uses the most efficient possible representation, identical to that of
``NativeArray``.

Components
----------

Swift provides three generic array types, all of which have amortized
O(1) growth.  In this document, statements about **ArrayType** apply
to all three of the components.

* ``NativeArray<T>`` is the fastest and simplest of the three—use this
  when you need "C array" performance.  The elements of a
  ``NativeArray`` are always stored contiguously in memory.

  .. image:: NativeArray.png

* ``Array<T>`` is like ``NativeArray<T>``, but optimized for efficient
  conversions from Cocoa and back—when ``T`` can be a class type,
  ``Array<T>`` can be backed by the (potentially non-contiguous)
  storage of an arbitrary ``NSArray`` rather than by a Swift
  ``NativeArray``.  ``Array<T>`` also supports up- and down- casts
  between arrays of related class types.  When ``T`` is known to be a
  non-class type, the performance of ``Array<T>`` is identical to that
  of ``NativeArray<T>``.

  .. image:: Array.png

* ``Slice<T>`` is a subrange of some ``Array<T>`` or
  ``NativeArray<T>``; it's the result of using slice notation,
  e.g. ``a[7...21]`` on any Swift array ``a``.  A slice always has
  contiguous storage and "C array" performance.  Slicing an
  *ArrayType* is O(1) unless the source is an ``Array<T>`` backed by
  an ``NSArray`` that doesn't supply contiguous storage.

  ``Slice`` is recommended for transient computations but not for
  long-term storage.  Since it references a sub-range of some shared
  backing buffer, a ``Slice`` may artificially prolong the lifetime of
  elements outside the ``Slice`` itself.

  .. image:: Slice.png

Mutation Semantics
------------------

Originally, the plan was to give *ArrayType*\ s full value semantics
via copy-on-write (COW).  However, that COW requires a check for
unique ownership before every write, which is only compatible with our
primary performance goal if those checks can be hoisted out of loops.
Since we can almost certainly not get hoisting uniqueness checks for
1.0, subscript assignments on an array will be visible through all
copies of that array::

  var a = [1, 2, 3]
  let b = a
  a[1] = 42
  println(b[1]) // prints "42"

This implies that the elments of an array are notionally not part of
the array's value, and indeed subscript assignment is a non-mutating
operation:

.. parsed-literal::

  **let** a = [1, 2, 3]
  **a[1] = 42** // OK

Unfortunately, full consistent reference semantics would also be
problematic with this design, because during array growth, at some
point available capacity is filled, and the array's buffer needs be
reallocated.  The only way to keep changes to the array visible
through its copies once the buffer is reallocated would be to add a
level of indirection between the arrays and their shared buffer, which
would conflict with our primary performance goals, requiring a hoist
optimization that we are again unlikely to get for 1.0.

Therefore, potentially size-changing operations such as ``append`` do
*not* have reference semantics, as they always (effectively) copy the
array to ensure unique ownership before mutating it::

  var a = [1, 2, 3]
  let b = a
  a[1] = 42
  println(b[1]) // prints "42"

Shared Subscript Assignment and ``NSArray``
-------------------------------------------

For ``Array`` there is one more wrinkle: when its storage is backed by
an immutable ``NSArray``, shared semantics for subscript assignment
implies that we add a level of indirection anyway: the NSArray needs
to be replaced by an array buffer containing the new value, and that
change needs to be visible through all copies of the array.  Remember
that this indirection has no cost in cases like ``Array<Int>``, where
it is statically known to be unneeded.

.. image:: ArrayBridge.png

``Array`` Casts
---------------

We can essentially reinterpret an ``Array`` buffer containing elements of
dynamic type ``Derived`` as a buffer of elements of type ``Base``,
where ``Derived`` is a subclass of ``Base``.  However, we cannot allow
arbitrary ``Base`` elements to be inserted in the buffer without
compromising type safety.  Also, our shared subscript assignment
semantics imply that all copies of the resulting ``Array<Base>``
see its subscript mutations.

Therefore, casting ``Array<T>`` to ``Array<U>`` is akin to resizing:
the new copy becomes independent.  To avoid an O(N) conversion cost,
we use a layer of indirection in the data structure.  The indirection
object is marked to prevent in-place mutation of the buffer; it will
be copied upon its first mutation:

.. image:: ArrayCast.png

The specific rules for casting are as follows:

* An ``Array<T>`` references a buffer of elements dynamically
  known to have type ``T``

* In O(1), ``Array<T>`` implicitly converts to ``Array<U>`` iff ``T``
  is derived from ``U`` or if ``T`` is *bridged* to ``U`` or a
  subclass thereof, including ``AnyObject``\ —see below__.  The
  resulting array references the same buffer as the original.

  __ `bridging to objective-c`_

* In O(1), ``Array<U>`` explicitly converts to ``Array<T>?`` via ``x
  as Array<T>``.  The cast succeeds, yielding a non-nil result, iff
  the array buffer elements are dynamically known to have type ``T``
  or a type derived from ``T``.  The resulting ``Array<T>`` references
  the same buffer as the original.

Bridging Rules and Terminology for all Types
--------------------------------------------

* An arbitrary Swift type ``T`` can conform to
  ``BridgedToObjectiveC``, which specifies its conversions to and from
  ObjectiveC::

    protocol _BridgedToObjectiveC {
      // FIXME: should be ': class' or ': AnyObject'
      typealias ObjectiveCType: ObjCClassType
      func bridgeToObjectiveC() -> ObjectiveCType
      class func bridgeFromObjectiveC(_: ObjectiveCType) -> Self?
    }

.. obsolete now that we potentially require indirection even for pure
   Swift classes

  * User-defined value types may conform to ``BridgedToObjectiveC``, but
    user-defined classes may not.  [This restriction allows us to
    maintain the highest efficiency for ``Array<T>`` where ``T`` is a
    Swift class].

* Some generic types (``Array<T>`` in particular) can be bridged only 
  if their element type can be bridged.  These conform to
  ``_ConditionallyBridgedToObjectiveC``::

    protocol _ConditionallyBridgedToObjectiveC : _BridgedToObjectiveC {
      class func isBridgedToObjectiveC() -> Bool
    }

* A type ``T`` is formally considered **bridged** (to type ``U``) if
  either:

  - ``T`` conforms to ``BridgedToObjectiveC`` and ``T.ObjectiveCType``
    is ``U`` and either
  
    - ``T`` does not conform to ``_ConditionallyBridgedToObjectiveC``
    - or, ``T.isBridgedToObjectiveC()`` is ``true``

  - or ``T`` is a class type that does not conform to
    ``BridgedToObjectiveC`` and ``T`` == ``U``.  In this case ``T`` is
    **bridged verbatim**.

* For a type ``T`` that is *bridged*, a value ``x`` of class type
  **bridges back** to ``T`` as the first of these values that is
  non-``nil``:

  - if ``T`` conforms to ``BridgedToObjectiveC``, ::
       T.bridgeFromObjectiveC((x as T.ObjectiveCType)!)

  - if ``T`` is a class type, ::
      ``(x as T)``

  Otherwise, ``x`` does not *bridge back* to ``T``.

Bridging To Objective-C
-----------------------

* *ArrayType*\ ``<T>`` is *bridged* to ``NSArray`` iff ``T`` is
  *bridged*.

* An ``NSArray`` can be constructed from any *bridged* ``Array<T>`` or
  ``NativeArray<T>`` in O(1), without allocation.  The elements of the
  resulting ``NSArray`` have dynamic type ``U``, where ``T`` is
  bridged to ``U``.

  .. Admonition:: Implementation Notes

     This works because the buffer held by ``Array<T>`` or
     ``NativeArray<T>`` always is-a ``NSArray``; we just extract it.
     We *could* also support construction of ``NSArray`` from
     ``Slice<T>`` at the cost of a single allocation

* Constructing an ``NSArray`` from an array of un-\ *bridged* element
  type is a fatal error detected at runtime.

  .. Admonition:: Implementation Note

     There are various ways to move this detection to compile-time
  
* if ``T`` is not *bridged verbatim*, the elements of the ``NSArray``
  (or ``Array<U>`` when bridging is used in array casting) are created
  once, on-demand, by calling ``bridgeToObjectiveC()`` on the original
  ``T``\ s, and will not be destroyed before the array from which they
  were extracted.  We cache these objects both in order to satisfy
  Cocoa users' expectations of stable element identity, and because
  ``NSArray`` APIs such as ``getObjects:range:`` are required to
  expose elements without lifetime management.

Bridging From Objective-C
-------------------------

* ``NSArray`` can be implicitly converted to ``Array<AnyObject>`` in
  O(1)

* ``NSArray`` and ``Array<AnyObject>`` can be *explicitly* converted
  to ``Array<T>?`` using ``a as Array<T>``.  There are several cases:

  - If ``T`` is not *bridged*, conversion fails in O(1), yielding nil

  - If the ``NSArray`` was originally created as a Swift *ArrayType*\
    ``<U>``, conversion succeeds in O(1) if ``U`` is ``T`` or a subclass
    thereof. No further dynamic type checks are required.

  - Otherwise, if ``T`` is a class or existential type, conversion
    succeeds in O(1), but type-checking of elements is deferred and
    on-demand.  The result of subscripting is the result of *bridging
    back* the corresponding stored ``NSArray`` element to ``T``.
    Failure to *bridge back* is a fatal error detected at runtime.

  - Otherwise, conversion is O(N), and succeeds iff every element
    *bridges back* to ``T``.
