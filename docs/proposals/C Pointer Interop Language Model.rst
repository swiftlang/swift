:orphan:

We have a pretty good user model for C pointer interop now, but the language
model still needs improvement. Building the user model on top of implicit
conversions has a number of undesirable side effects. We end up with a mess of
pointer types—the intended user-facing, one-word pointer types
``UnsafePointer`` and ``COpaquePointer``, which expose a full pointer-ish API
and are naturally ABI-compatible with C pointers; and the bridging pointer
types, ``ObjCMutablePointer``, ``CMutablePointer``, ``CConstPointer``,
``CMutableVoidPointer``, and ``CConstVoidPointer``, which have no real API yet
but exist only to carry an owner reference and be implicitly convertible, and
rely on compiler magic to be passed to C functions. Since we can do the magic
pointer bridging only in limited places, we assault users writing method
overrides and reading synthesized headers with both sets of pointer types in a
confusing jumble.

The best solution to this is to burn the user model into the language, giving
function applications special powers to provide the user model for pointers. We
then provide only one set of plain pointer types, with nice C-ish sugar, and
special intrinsic behavior when used as function arguments.

The Pointer Types
=================

In the standard library, we provide three pointer types:

- ``ConstUnsafePointer<T>``, corresponding to ``T const *`` in C and ARC,
- ``UnsafePointer<T>``, corresponding to ``T *`` in C, and ``T* __strong *`` in
  ARC for class types, and
- ``AutoreleasingUnsafePointer<T>`` (for all ``T: AnyObject``), corresponding
  to ``T* __autoreleasing *`` in ARC.

These types are all one word, have no ownership semantics, and share a common
interface. ``ConstUnsafePointer`` does not expose operations for storing to the
referenced memory. ``UnsafePointer`` and ``AutoreleasingUnsafePointer`` differ
in store behavior: ``UnsafePointer`` assumes that the pointed-to reference has
ownership semantics, so ``ptr.initialize(x)`` consumes a reference to ``x``,
and ``ptr.assign(x)`` releases the originally stored value before storing the
new value.  ``AutoreleasingUnsafePointer`` assumes that the pointed-to
reference does not have ownership semantics, so values are autoreleased before
being stored by either initialize() or assign(), and no release is done on
reassignment. Loading from any of the three kinds of pointer does a strong
load, so there is no need for a separate ``AutoreleasingConstUnsafePointer``.

Sugar for Pointer Types
-----------------------

We add syntactic sugar for the pointer types:

- ``T*`` by itself is sugar for ``UnsafePointer<T>``.
- ``@const T*`` is sugar for ``ConstUnsafePointer<T>``.

To avoid blowing ObjC programmers' minds, we prevent either sugar from being
applied to class types, since ``NSObject*`` in Swift would be the equivalent of
the very different type ``NSObject* __strong *`` in ObjC. We won't provide any
sugar for ``AutoreleasingUnsafePointer<T>`` at first, except for a type alias
for the common pointer-to-NSError case::

  typealias NSErrorResult = AutoreleasingUnsafePointer<NSError>

Conversion behavior for pointer arguments
=========================================

The user model for pointer arguments becomes an inherent capability of function applications. The rules are:

UnsafePointer<T>
----------------

When a function is declared as taking an ``UnsafePointer<T>`` argument, it can
accept any of the following:

- ``nil``, which is passed as a null pointer,
- an ``UnsafePointer<T>`` value, which is passed verbatim,
- an inout expression whose operand is a stored lvalue of type ``T``, which is
  passed as the address of the lvalue, or
- an inout ``Array<T>`` value, which is passed as a pointer to the start of the
  array, and lifetime-extended for the duration of the callee.

As a special case, when a function is declared as taking an
``UnsafePointer<Void>`` argument, it can accept the same operands as
``UnsafePointer<T>`` for any type T.

So if you have a function declared::

  func foo(x: Float*)

You can call it as any of::

  var x: Float = 0.0
  var p: Float* = nil
  var a: Float[] = [1.0, 2.0, 3.0]
  foo(nil)
  foo(p)
  foo(&x)
  foo(&a)

And if you have a function declared::

  func bar(x: Void*)

You can call it as any of::

  var x: Float = 0.0, y: Int = 0
  var p: Float* = nil, q: Int* = nil
  var a: Float[] = [1.0, 2.0, 3.0], b: Int = [1, 2, 3]
  bar(nil)
  bar(p)
  bar(q)
  bar(&x)
  bar(&y)
  bar(&a)
  bar(&b)

AutoreleasingUnsafePointer<T>
-----------------------------

When a function is declared as taking an ``AutoreleasingUnsafePointer<T>``, it
can accept any of the following:

- nil, which is passed as a null pointer,
- an ``AutoreleasingUnsafePointer<T>`` value, which is passed verbatim, or
- an inout expression, whose operand is primitive-copied to a temporary
  nonowning buffer. The address of that buffer is passed to the callee, and on
  return, the value in the buffer is loaded, retained, and reassigned into the
  operand.

Note that the above list does not include arrays, since implicit autoreleasing-to-strong writeback of an entire array would probably not be desirable.

So if you have a function declared::

  func bas(x: AutoreleasingUnsafePointer<NSBas?>)

You can call it as any of::

  var x: NSBas? = nil
  var p: AutoreleasingUnsafePointer<NSBas?> = nil
  bas(nil)
  bas(p)
  bas(&x)

ConstUnsafePointer<T>
---------------------

When a function is declared as taking an ``UnsafePointer<T>`` argument, it can
accept any of the following:

- nil, which is passed as a null pointer,
- an ``UnsafePointer<T>``, ``ConstUnsafePointer<T>``, or
  ``AutoreleasingUnsafePointer<T>`` value, which is converted to
  ``ConstUnsafePointer<T>`` if necessary and passed verbatim,
- an inout expression whose operand is an lvalue of type ``T``, which is passed
  as the address of (the potentially temporary writeback buffer of) the lvalue,
  or
- an ``Array<T>`` value, which is passed as a pointer to the start of the
  array, and lifetime-extended for the duration of the callee.

As a special case, when a function is declared as taking an
``ConstUnsafePointer<Void>`` argument, it can accept the same operands as
``ConstUnsafePointer<T>`` for any type ``T``.

So if you have a function declared::

  func zim(x: @const Float*)

You can call it as any of::

  var x: Float = 0.0
  var p: @const Float* = nil
  zim(nil)
  zim(p)
  zim(&x)
  zim([1.0, 2.0, 3.0])

And if you have a function declared::

  func zang(x: @const Void*)

You can call it as any of::

  var x: Float = 0.0, y: Int = 0
  var p: @const Float* = nil, q: @const Int* = nil
  zang(nil)
  zang(p)
  zang(q)
  zang(&x)
  zang(&y)
  zang([1.0, 2.0, 3.0])
  zang([1, 2, 3])
