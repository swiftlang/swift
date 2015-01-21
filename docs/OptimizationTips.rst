:orphan:

Writing Performant Swift Code
=============================

The following is a gathering of various tips and tricks for writing performance
sensitive Swift code.

Enabling Optimizations
======================

The first thing one should always do is to enable optimization. Swift provides
three different optimization levels:

- ``-Onone``: This is meant for normal development. It performs minimal
  optimizations and preserves all debug info.
- ``-O``: This is meant for most production code. The compiler performs
  aggressive optimizations that can drastically change the type and amount of
  emitted code. Debug information will be emitted but will be lossy.
- ``-Ounchecked``: This is a special optimization mode meant for specific
  libraries or applications where one is willing to trade safety for
  performance.  The compiler will remove all overflow checks as well as some
  implicit type checks.  This is not intended to be used in general since it may
  result in undetected memory safety issues and integer overflows. Only use this
  if you have carefully reviewed that your code is safe with respect to integer
  overflow and type casts.

In the Xcode UI, one can modify the current optimization level as follows:

...

Limiting Language Dynamicism
============================

Swift by default is a very dynamic language like Objective-C. Unlike Objective
C, Swift gives the programmer the ability to improve runtime performance when
necessary by removing or reducing this dynamicism. This section goes through
several examples of language constructs that can be used to perform such an
operation.

Dynamic Dispatch
----------------

Classes use dynamic dispatch for methods and property accesses by default. Thus
in the following code snippet, ``a.aProperty``, ``a.doSomething()`` and
``a.doSomethingElse()`` will all be invoked via dynamic dispatch:

::

  class A {
    var aProperty: [Int]
    func doSomething() { ... }
    dynamic doSomethingElse() { ... }
  }

  class B : A {
    override var aProperty {
      get { ... }
      set { ... }
    }

    override func doSomething() { ... }
  }

  func usingAnA(a: A) {
    a.doSomething()
    a.aProperty = ...
  }

In Swift, dynamic dispatch defaults to indirect invocation through a vtable
[#]_. If one attaches the ``dynamic`` keyword to the declaration, Swift will
emit calls via Objective-C message send instead. In both cases this is slower
than a direct function call because it prevents many compiler optimizations [#]_
in addition to the overhead of performing the indirect call itself. In
performance critical code, one often will want to restrict this dynamic
behavior.

Advice: Use 'final' when you know the declaration does not need to be overridden
--------------------------------------------------------------------------------

The ``final`` keyword is a restriction on a declaration of a class, a method, or
a property such that the declaration cannot be overridden. This implies that the
compiler can emit direct function calls instead of indirect calls. For instance
in the following ``C.array1`` and ``D.array1`` will be accessed directly
[#]_. In contrast, ``D.array2`` will be called via a vtable:

::

  final class C {
    // No declarations in class 'C' can be overridden.
    var array1: [Int]
    func doSomething() { ... }
  }

  class D {
    final var array1 [Int] // 'array1' cannot be overridden by a computed property.
    var array2: [Int]      // 'array2' *can* be overridden by a computed property.
  }

  func usingC(c: C) {
     c.array1[i] = ... // Can directly access C.array without going through dynamic dispatch.
     c.doSomething() = ... // Can directly call C.doSomething without going through virtual dispatch.
  }

  func usingD(d: D) {
     d.array1[i] = ... // Can directly access D.array1 without going through dynamic dispatch.
     d.array2[i] = ... // Will access D.array2 through dynamic dispatch.
  }

Advice: Use 'private' when declaration does not need to be accessed outside of file
-----------------------------------------------------------------------------------

Applying the ``private`` keyword to a declaration restricts the visibility of
the declaration to the file in which it is declared. This allows the compiler to
be able to ascertain all other potentially overridding declarations. Thus the
absense of any such declarations enables the compiler to infer the ``final``
keyword automatically and remove indirect calls for methods and field accesses
accordingly. For instance in the following, ``e.doSomething()`` and
``f.myPrivateVar``, will be able to be accessed directly assuming ``E``, ``F``
do not have any overridding declarations in the same file:

::

  private class E {
    func doSomething() { ... }
  }

  class F {
    private var myPrivateVar : Int
  }

  func usingE(e: E) {
    e.doSomething() // There is no sub class in the file that declares this class.
                    // The compiler can remove virtual calls to doSomething()
                    // and directly call A’s doSomething method.
  }

  func usingF(f: F) -> Int {
    return f.myPrivateVar
  }

Using Container Types Efficiently
=================================

An important feature provided by the Swift standard library are the generic
containers Array and Dictionary. This section will explain how to use these
types in a performant manner.

Advice: Use value type for small POD types in containers
--------------------------------------------------------

In Swift, types can be divided into two different categories: value types
(structs) and reference types (classes). While there are many distinctions in
between the two semantic wise from a container stand point the main interesting
item is that value types can not be Objective-C classes. Inside the Swift
standard library containers, there is much special code to handle bridging from
Objective-C. By using a value type, one can avoid all such bridging code.

Additionally, In contrast to reference types, value types only need reference
counting if they contain recursively a reference type themselves. This avoids
additional retain, release traffic inside Array.

If you can choose between a class and a struct use a struct if the copy of the
whole struct is expected to be a cheap operation.

::

  // Don't use a class here.
  struct PhonebookEntry {
    final var name : String
    final var number : [Int]
  }

  var a : [PhonebookEntry]

Advice: Use inout to prevent unnecessary COW copies
---------------------------------------------------

Arrays and Dictionaries in Swift are value types that use COW (copy-on-write)
[#]_ to perform copies instead of explicitly copies. In many cases this allows
the compiler to elide unnecessary copies by retaining the container instead of
performing a deep copy.  In other cases, unexpected copies can occur. One common
case that can cause unexpected copies is if one passes an array as a parameter
to a function. This causes the reference count of the array to be incremented
causing a copy if one mutates the array. For instance in the following, the call
to append will cause an unnecessary copy of ``a``, while the call to
inplace_append will not cause any copies:

::

  func append(a: [Int], value: Int) -> [Int] {
    a.append(value)
    return a
  }

  func inplace_append(inout a: [Int], value: Int) {
    a.append(value)
  }

  var a = [ ... ]
  a = append(a, 0)
  inplace_append(a, 1)

Unchecked operations
====================

One source of bugs that Swift eliminates are integer overflows. This is done by
performing checks for overflow when performing normal arithmetic. This is not
appropriate in high performance code where one knows that no memory safety
issues can result.

Advice: Use unchecked integer operations when its is known to be safe
---------------------------------------------------------------------

In performance critical code you can elide overflow checks if you know it is
safe.

::

  a : [Int]
  b : [Int]
  c : [Int]

  // Precondition: for all a[i], b[i]: a[i] + b[i] does not overflow!
  for i in 0 ... n {
    c[i] = a[i] &+ b[i]
  }

Generics
========

Swift provides a very powerful abstraction mechanism through the use of generic
types. By default, the Swift compiler will generate generic code that can be
passed any type through the usage of boxes and vtables. Thus in the following,
``MySwiftFunc`` only emits one generic function for use in both
``MySwiftFunc<Int>`` and ``MySwiftFunc<Double>``.

::

  class MySwiftFunc<T> { ... }

  MySwiftFunc<Int> X    // Will instantiate generic code that works with Int...
  MySwiftFunc<Double> Y // ... as well as Double.

This contrasts to other languages like C++ where generic templates are
instantiated for every use, i.e.,

::

  template <typename T>
  class MyCPPFunc<T> { ... };

  MyCPPFunc<int> X;     // Will instantiate MyCPPFunc<int>.
  MyCPPFunc<double> Y;  // Will instantiate MyCPPFunc<double>.

When optimizations are enabled, the optimizer looks at each invocation of such
code and attempts to ascertain the concrete (i.e. non-generic type) use in the
invocation. If the generic function's definition is visible to the optimizer and
the concrete type is known, the Swift compiler will emit a version of the
generic function specialized to the specific type. This process, called
*specialization*, enables the removal of the overhead associated with
generics. Some more examples of generics:

::

  class MyStack<T> {
    func push(element: T) { ... }
    func pop() -> T { ... }
  }

  func myAlgorithm(a: [T], length: Int) { ... }

  // The compiler can specialize code of MyStack[Int]
  var stackOfInts: MyStack[Int]
  // Use stack of ints.
  for i in ... {
    stack.push(...)
    stack.pop(...)
  }

  var arrayOfInts: [Int]
  // The compiler can emit a specialized version of 'myAlgorithm' targeted for
  // [Int]' types.
  myAlgorithm(arrayOfInts, arrayOfInts.length)

Advice: Put generic declarations in the same file where they are used
---------------------------------------------------------------------

The optimizer can only perform specializations if the definition of the generic
declaration is visible in the current Module. This can only occur if the
declaration is in the same file as the invocation of the generic.

Footnotes
=========

.. [#] A virtual method table or 'vtable' is a type specific table referenced by
     instances that contains the addresses of the type's methods.  Dynamic dispatch
     proceeds by first looking up the table from the object and then looking up the
     method in the table.

.. [#] This is due to the compiler not knowing the exact function being called.

.. [#] i.e. a direct load of a class's field or a direct call to a function.

.. [#] Explain what COW is here.
