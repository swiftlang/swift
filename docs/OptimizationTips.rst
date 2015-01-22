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

Advice: Use 'final' when one knows the declaration does not need to be overridden
---------------------------------------------------------------------------------

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
absence of any such declarations enables the compiler to infer the ``final``
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

Advice: Use value types in Array
--------------------------------

In Swift, types can be divided into two different categories: value types
(structs, enums, tuples) and reference types (classes). A key distinction is
that value types can not be included inside an NSArray. Thus when using value
types, the optimizer can remove most of the overhead in Array that is necessary
to handle ContiguousArray and NSArray.

Additionally, In contrast to reference types, value types only need reference
counting if they contain, recursively, a reference type. By using value types
without reference types, one can avoid additional retain, release traffic inside
Array.

::

  // Don't use a class here.
  struct PhonebookEntry {
    var name : String
    var number : [Int]
  }

  var a : [PhonebookEntry]

Keep in mind that there is a trade-off between using large value types and using
reference types. In certain cases, the overhead of copying and moving around
large value types will outweigh the cost of removing the bridging and
retain/release overhead.

Advice: Use inplace mutation instead of object-reassignment
-----------------------------------------------------------

All standard library containers in Swift are value types that use COW
(copy-on-write) [#]_ to perform copies instead of explicit copies. In many cases
this allows the compiler to elide unnecessary copies by retaining the container
instead of performing a deep copy. This is done by only copying the underlying
container if the reference count of the container is greater than 1 and the
container is mutated. For instance in the following, no copying will occur when
``d`` is assigned to ``c``, but when ``d`` undergoes structural mutation by
appending ``2``, ``d`` will be copied and then ``2`` will be appended to ``d``:

::

  var c: [Int] = [ ... ]
  var d = c        // No copy will occur here.
  d.append(2)      // A copy *does* occur here.

Sometimes COW can introduce additional unexpected copies if the user is not
careful. An example of this is attempting to perform mutation via
object-reassignment in functions. In Swift, all parameters are passed in at +1,
i.e. the parameters are retained before a callsite, and then are released at the
end of the callee. This means that if one writes a function like the following:

::

  func append_one(a: [Int]) -> [Int] {
    a.append(1)
    return a
  }

  var a = [1, 2, 3]
  a = append_one(a)

``a`` may be copied [#]_ despite the version of ``a`` without one appended to it
has no uses after ``append_one`` due to the assignment. This can be avoided
through the usage of ``inout`` parameters:

::

  func append_one_in_place(inout a: [Int]) {
    a.append(1)
  }

  var a = [1, 2, 3]
  append_one_in_place(&a)

Unchecked operations
====================

Swift eliminates integer overflow bugs by checking for overflow when performing
normal arithmetic. These checks are not appropriate in high performance code
where one knows that no memory safety issues can result.

Advice: Use unchecked integer when not compromising memory safety
-----------------------------------------------------------------

In performance-critical code you can elide overflow checks if you know it is
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
types. The Swift compiler emits one block of concrete code that can perform
``MySwiftFunc<T>`` for any ``T``. The generated code takes a table of function
pointers and a box containing ``T`` as additional parameters. Any differences in
behavior between ``MySwiftFunc<Int>`` and ``MySwiftFunc<String>`` are accounted
for by passing a different table of function pointers and the size abstraction
provided by the box. An example of generics:

::

  class MySwiftFunc<T> { ... }

  MySwiftFunc<Int> X    // Will emit code that works with Int...
  MySwiftFunc<String> Y // ... as well as String.

When optimizations are enabled, the Swift compiler looks at each invocation of
such code and attempts to ascertain the concrete (i.e. non-generic type) used in
the invocation. If the generic function's definition is visible to the optimizer
and the concrete type is known, the Swift compiler will emit a version of the
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
declaration is in the same file as the invocation of the generic. *NOTE* The
standard library is a special case. Definitions in the standard library are
visible in all modules and available for specialization.


Unsafe code
===========

Swift classes are always reference counted. The swift compiler inserts code
that increments the reference count every time the object is accessed.
For example, consider the problem of scanning a linked list that's
implemented using classes. Scanning the list is done by moving a
reference from one node to the next: ``elem = elem.next``. Every time we move
the reference swift will increment the reference count of the ``next`` object
and decrement the reference count of the previous object. These reference
count operations are expensive and unavoidable when using Swift classes.

::

  final class Node {
   var next: Node?
   var data: Int
   ...
  }


Swift provided a way for working with C pointers and raw memory. These
unsafe constructs are fast and powerful, but are also error prone. It is
possible to implement the linked list object using unsafe constructs and
scan the list using unsafe pointers.

::

  struct Node {
    var next : UnsafeMutablePointer<Node> = nil
    var data : Int = 0
  ...
  }



Advice: Use unsafe pointers to avoid reference counting overhead
----------------------------------------------------------------

In performance-critical code you can use choose to use unsafe pointers
and manage the memory yourself.

::

    // Allocate
    var x = UnsafeMutablePointer<Node>.alloc(sizeof(Node))

    // Mutate
    x.memory.next = y

    // Free
    x.destroy()

Footnotes
=========

.. [#] A virtual method table or 'vtable' is a type specific table referenced by
       instances that contains the addresses of the type's methods. Dynamic
       dispatch proceeds by first looking up the table from the object and then
       looking up the method in the table.

.. [#] This is due to the compiler not knowing the exact function being called.

.. [#] i.e. a direct load of a class's field or a direct call to a function.

.. [#] Explain what COW is here.

.. [#] In certain cases the optimizer is able to via inlining and ARC
       optimization remove the retain, release causing no copy to occur.
