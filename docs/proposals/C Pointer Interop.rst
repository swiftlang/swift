:orphan:

Summary
=======

This proposal calls for the following conventions for parameters of C and
Objective-C functions:

- Non-const pointer arguments ``T *`` can be used as ``UnsafePointer<T>``,
  ``@inout T``, or ``@inout Array<T>`` arguments.
- Const pointer arguments ``const T *`` can be used as ``UnsafePointer<T>``,
  ``T`` or ``Array<T>`` arguments.

(Where this document refers to "pointers", it means "non-ObjC-object pointers"
unless otherwise stated.)

Pointer arguments are pervasive in C and Objective-C, and for Objective-C
parity, we must be able to interface with Cocoa APIs that use them.
However, without labor-intensive annotations in the SDKs, we can't infer
anything about what a pointer argument means in C. Is it expected to point to
initialized data?  Does it point to a single object or an array of objects?
Can it be null? This makes it practically impossible for the Clang
importer to synthesize an accurate, safe Swift interface for a function taking
pointer arguments.

Instead of trying to guarantee absolute safety, this proposal calls for an
approach that both accommodates low-level use of pointers and also enables
well-behaved ObjC and C APIs to be used in a more Swift-like way.

This proposal only describes an interface the language should provide for
working with C pointer arguments from Swift; it intentionally does not
describe any implementation details, such as language or library features,
that would help realize the proposal.

Inout to Non-Const Pointer Conversion
=====================================

Non-const pointer parameters to C or ObjC functions can be used with ``@inout``
arguments. This allows well-behaved C or ObjC APIs that use
pointers as inout parameters to be used naturally in Swift. For example::
  
  // ObjC
  @interface Foo
  + increment:(NSInteger *)x;
  @end

  // Swift
  import Foo
  var x = 0
  Foo.increment(&x)

This is safe only if the following conditions hold in the callee:

- The value type is POD or an Objective-C class pointer.
- The callee does not capture the pointer value passed for the inout parameter.
  The referenced memory cannot be read to or written from after the end of
  the call.
- The callee does not access any memory relative to the pointer outside of the
  pointed-to value; that is, it does not expect an array of more than one
  object at the pointed-to address.

If the inout parameter is a logical property, then writeback proceeds as for
native Swift functions: a temporary allocation is materialized with the result
of the getter prior to the call, and after the call, the value at the address
is loaded and passed to the setter.

As for Swift inout arguments, the argument is required to be initialized prior
to the call::

  import Foo
  var y: Int
  // Error, y not initialized before use
  Foo.increment(&y)

If the pointed-to type is an Objective-C class pointer type, the ownership
attributes of the argument, such as ``__autoreleasing``, ``__strong``, etc.
are honored during the duration of the call.

Value to Const Pointer Conversion
=================================

Const pointer arguments to C or ObjC functions can be given a value of the
pointed-to type. The value is implicitly materialized to a temporary buffer,
and a pointer to the buffer is passed to the callee. This allows well-behaved
C or ObjC APIs that use const pointers for 'in' parameters to be used naturally
in Swift. For example::

  // ObjC
  struct Tone {
    double bass;
    double mid;
    double treble;
    double gain;
  }

  @interface Amplifier
  - init;
  - setTone:(const Tone*)tone;
  @end

  // Swift
  import Amplifier
  var t = Amplifier()
  t.setTone(Tone(bass: 0.5, mid: 0.8, treble: 0.9, gain: 0.6))

This is safe only if the following conditions hold in the callee:

- The value type is POD or an Objective-C class pointer.
- The callee does not capture the pointer value; the referenced memory is
  invalidated at the end of the call.
- The callee does not write to the referenced memory.
- The callee does not access memory relative to the pointer outside of the
  pointed-to value; that is, it does not expect an array of more than one
  object at the pointed-to address.

If the pointed-to type is an Objective-C class pointer type, the ownership
attributes of the argument, such as ``__autoreleasing``, ``__strong``, etc.
are honored during the duration of the call.

Array to Pointer Conversion
===========================

Pointer parameters to C or ObjC functions of type ``T *`` or ``const T *`` can
be used with Swift ``Array``\ s. If the parameter is const, it can be given
an ``Array`` value argument, if it is non-const, it can be given an ``@inout``
value argument. This is safe under similar conditions to scalar value
arguments:

- The element type is POD or an Objective-C class pointer.
- The callee does not capture the pointer value; the referenced memory is
  invalidated at the end of the call.
- If the parameter is ``const``, the callee cannot write to the referenced
  memory.
- The callee cannot resize the array, and cannot reference memory outside of
  the array.

If the pointed-to type is an Objective-C class pointer type, the ownership
attributes of the argument, such as ``__autoreleasing``, ``__strong``, etc.
are honored for the elements in the array during the duration of the call.

