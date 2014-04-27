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

Introductory Blather
--------------------

Components
----------

Swift provides three generic array types, all of which have
copy-on-write value semantics and amortized O(1) growth.  In this
document, statements about **ArrayType** apply to all three of the
components.

* ``NativeArray<T>`` is the fastest and simplest of the three—use this
  when you need "C array" performance.  The elements of a
  ``NativeArray`` are always stored contiguously in memory.

* ``Array<T>`` is like ``NativeArray<T>``, but optimized for efficient
  conversions from Cocoa and back—when ``T`` is a pure Objective-C
  class or protocol, it can be backed by the (potentially
  non-contiguous) storage of an arbitrary ``NSArray`` rather than by a
  Swift ``NativeArray``.  When ``T`` is known to be a pure Swift type,
  the performance of ``Array<T>`` is identical to that of
  ``NativeArray<T>``.

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

Array Casts
-----------

* *ArrayType*\ ``<T>`` implicitly converts to *ArrayType*\ ``<U>`` if
  ``T`` is a trivial subtype of ``U`` (or if ``U`` is ``AnyObject``\
  —see below).  [Implementation note: when accessed as *ArrayType*\
  ``<U>``, the underlying buffer of ``T``\ s is treated as immutable,
  to be copied-on-write, even if uniquely-referenced]

* *ArrayType*\ ``<U>`` explicitly converts to *ArrayType*\ ``<T>``?
  via ``x as ArrayType<T>``.  The cast succeeds, yielding a non-nil
  result, iff the original value was of some *ArrayType*\ ``<V>``
  where ``V`` is a trivial subtype of ``T``. [Implementation note: if
  ``V`` == ``T``, the underlying buffer need only be treated as
  immutable if uniquely referenced]

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

* A type ``T`` is formally considered **bridged** if:

  - it conforms to ``_ConditionallyBridgedToObjectiveC`` and
    ``T.isBridgedToObjectiveC()`` is ``true``

  - or, ``T`` does not conform to
    ``_ConditionallyBridgedToObjectiveC``, and

    + it is a class type, or
    + it conforms to ``BridgedToObjectiveC``

* A type ``T`` is considered **bridged verbatim** if it is a class
  type that does not conform to ``BridgedToObjectiveC``

* A value ``x`` of type AnyObject **bridges** to ``T`` if ``T`` is
  *bridged* and ::

     T.bridgeFromObjectiveC((x as T.ObjectiveCType)!)

  is valid and non-nil.

Bridging To Objective-C
-----------------------

* *ArrayType*\ ``<T>`` is *bridged* iff ``T`` is *bridged*.  

* ``NSArray`` can be constructed from any *bridged* ``Array<T>`` or
  ``NativeArray<T>`` in O(1), without allocation.  [Implementation
  note: we could allow construction from ``Slice<T>`` also, but it
  would require an allocation]

* Any *bridged* *ArrayType*\ ``<T>`` is implicitly convertible to
  *ArrayType*\ ``<AnyObject>`` in O(1), without allocation.

* In both cases above, if ``T`` is not *bridged*, it is considered a
  fatal error, detected at runtime.

* if ``T`` is not *bridged verbatim*, the elements of the resulting
  ``NSArray`` or ``Array<AnyObject>`` are created once, on-demand, by
  calling ``bridgeToObjectiveC()`` on the original ``T``\ s, and will
  be kept alive as long as does the array from which they were
  extracted.

Bridging From Objective-C
-------------------------

* ``NSArray`` can be implicitly converted to ``Array<AnyObject>`` in
  O(1)

.. What about ``Array<Any>``?  Do we care?

* ``NSArray`` and ``Array<AnyObject>`` can be *explicitly* converted
  to ``Array<T>?`` using ``a as Array<T>``.  There are several cases:

  - If the ``NSArray`` was originally created as a Swift 
    *ArrayType*\ ``<U>``, conversion is O(1) and succeeds if ``U`` 
    is a trivial subtype of ``T`` 

  - Otherwise, if ``T`` is not *bridged*, conversion fails in O(1),
    yielding nil

  - Otherwise, if ``T`` is a pure ObjC class or protocol, conversion
    succeeds unconditionally in O(1).  In that case, any individual
    element that is not actually a ``T`` may be detected later by the
    usual Objective-C means, most commonly an ``objc_msgSend``
    failure.

  - Otherwise, conversion is O(N) and succeeds iff every element of
    the ``NSArray`` *bridges* to ``T``.
