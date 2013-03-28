.. @raise litre.TestsAreMissing
.. _MemoryAndOwnershipModel:

Swift Memory and Ownership Model
================================

Memory Layout of Typed Values
-----------------------------

Swift has both value types and reference types, where a value type is copied
around by-value (and modifying copies doesn't change the original). Reference
types are basically a pointer to the underlying type. The pointer is passed
around by value, but this leads to sharing of the pointed-to value. Values types
for local variables in functions are nominally stored on the stack or in
registers (though can be promoted to the heap, as in ObjC blocks).

There are many simple values types: tuples, integers and floating point values,
and sized array values are all value types. For example::

  func foo() {
    var myarray : int[42];
  }

declares an array of 42 integers on the stack, there is no heap allocation
involved.

Objects (when we figure out what they look like) are always implicitly reference
types. Functions and array slices are reference types. As a silly example::

  func foo() {
    var myarray : int[42];
    var arr : int[] = myarray; // Reference to myarray.
    arr[4] = 1; // Updates myarray.
  }

Aggregates (structs and oneof) default to being value types, but can be declared
as reference types with the byref attribute::

  struct [byref] MyList {
    data : int,
    next : MyList
  }

The eventual "any" type and protocol types are reference values that store small
values directly inside themselves and autobox larger values. We'll eventually
support a real "pointer type" (probably with a "*") which are only used for
pointers to non-swift C datatypes.

All reference types should be optionally qualified with the 'weak' modifier. A
weak reference drops to null if the underlying reference value is destroyed
while the reference is still live. This qualifier can be represented
syntactically in several forms, but the actual spelling of it isn't important
right now.

Memory Management Approach and Ownership Model
----------------------------------------------

Swift provides automatic memory management with deterministic object lifecycle
and predictable destruction through an Automated Reference Counting (ARC)
approach. When the last strong reference to an reference value is removed it is
destroyed and any uses of weak references to it produce a null value.

Compared to manual memory management (ala new/delete, malloc/free,
retain/release), automatic reference counting eliminates the possibility of
dangling pointer bugs, substantially reduces occurrences of memory leaks, and is
much simpler to learn and less code to write. On the other hand, there is more
inherent refcount overhead to cannot be avoided (we hope the optimizer will
remove much of it).

Compared to accurate garbage collection (ala Java or .NET), ARC provides
deterministic object destruction (thus able to support RAII idioms), reclaims
objects faster (thus has a lower average amount of memory allocated), does not
have finalization (or resurrection, or other confusing semantics), does not
suffer from unpredictable pauses, and is simpler to implement. ARC is also much
more reasonable to use in space constrained environments like firmware and
kernels than a full GC. On the other hand, basic ARC can leak with cyclic strong
references and does not compact the heap (so it may have higher memory footprint
in some cases). Adding a cycle collector + compactor to ARC may be an
interesting later project.

Compared to conservative GC (ala ObjC libauto), ARC does not suffer from leaks
caused by conservative stack scanning or subtle bugs due to lost write barriers
etc. Otherwise it is similar to the normal GC case.

Runtime Implementation and Moving Parts
---------------------------------------

Many of the details are up for debate and change, but the best approach is to
start with something simple and implementable, then experiment with variants
when we have enough code working to do performance measurements. Here is one
possible implementation approach to start with:

Each reference value is stored in the swift heap, and they each have an object
header word containing a refcount and some bits for the runtime to play
with. The refcount reflects the number of strong references to the object. If
there are any weak references to the object, a "has weak references" bit is set
in the object header and a count of the weak references is stored in an
on-the-side hashtable.

When a reference is copied into a strong reference, the copy decrements the
strong refcount of the overwritten reference and increments the refcount of the
new pointee. When a reference is copied into a weak reference, the weak refcount
of the old pointer is reduced and the weak refcount of the new pointer is
incremented (allocating a hash table entry and setting the "has weak ref" bit in
the object header if needed). When memory is allocated, it is returned from the
allocation function and has a refcount of 1 (the returned pointer).

When a strong refcount goes to zero, the object is dead. The first step is to
run the object destructor if it exists or to null out the object if it
doesn't. This can cause recursive destruction of objects.  After the object is
destroyed, the on-the-side hash table is checked to see if there are any weak
references, if not, the memory is freed.  Otherwise the memory is left allocated
(but has been destructed).  Whenever a weak ref count goes to zero, a check is
done to see if the strong ref count of the object is already zero. If so, the
(already destructed) memory for the object is deallocated.

One annoying part of using ARC instead of GC is that references to heap objects
always have to get to the refcount. I don't think that this requirement affects
anything other than array references (int[]), which are effectively inner
pointers into an existing object or stack.  While originally envisioned to be
two words (pointer + element count), the array reference representation now
needs to be three words: pointer, element count, containing object pointer.

Runtime Optimizations
---------------------

As described, ARC would be very expensive at runtime, continuously atomically
diddling around with refcounts. The optimizer should aim to eliminate as many
redundant refcounts manipulations as it can (and is one more reason that neither
the strong nor the weak refcount should be exposed/accessible to user
code). This can clean up a lot of redundant manipulation from local variables on
the stack and trivial getters, for example.



