Swift Intermediate Language
===========================

Abstract
--------

TODO: enable high-level optimization, interoperability, persistence, embedded
interpreter, second coming, etc.

Structure
---------

SIL is an SSA-form IR similar to LLVM assembly language. Values represent
virtual registers and are immutable once instantiated. Mutation is represented
by loading and storing to allocated memory as in LLVM. However, unlike LLVM,
SIL represents branches using functional representation; rather than use phi
nodes to reconcile values in branches, basic blocks resemble functions that
implicitly close over their dominating blocks, and branch instructions look
like function calls. Every SIL instruction also carries a reference back into
the originating Swift AST for diagnostic purposes. The first basic block of a
function takes the function's arguments as its own.

In Swift, memory management is almost always implicit, but in SIL, it is always
explicit. Allocation, deallocation, and reference counting have explicit
instructions in SIL, and instructions such as tuple construction and function
calls in SIL never implicitly retain or release objects even if the analogous
high-level operations in Swift do.

Types
-----

SIL's type system is Swift's with some additional aspects. Like Swift, there
are two broad categories of types based on value semantics:

* *reference types*, which are handles to reference-counted boxes and are
  stored and passed around by reference, for example, classes and functions.
* *value types*, which are stored in-line and passed by value, for example,
  structs, tuples, and primitive types.

SIL classifies types into two additional subgroups based on ABI stability:

* *loadable types* are types with a fully exposed concrete representation. All
  reference types are loadable. Value types are loadable if they are fragile
  and all of their component types are fragile.
* *address-only types* are value types for which the compiler cannot access a
  full concrete representation, such as resilient value types or types that
  contain resilient types. Memory that contains an address-only type may be
  referred to by an address, but those addresses cannot be directly loaded or
  stored.

SIL adds some additional types of its own, which are not first-class Swift types but are needed
to :

* The *address of T* ``Address<T>``, a pointer to memory containing a value of
  any reference or value type ``T``.  This can be an internal pointer into a
  data structure. Addresses of loadable types can be loaded and stored to
  access values of those types. Addresses of address-only types can only be
  used with the ``copy``, ``destroy``, and ``dealloc`` instructions, or as
  arguments to functions. Addresses cannot be retained or released.
* The *object pointer* ``ObjectPointer`` is a generic pointer to a retainable
  block of memory, or *box*. An object pointer behaves like a reference type,
  but its contents are not accessible; it can only be retained, released, or
  passed around. Operations that allocate retainable memory generally return
  both an object pointer for the box and an address pointing to the object or
  objects inside the box.
* Unlike Swift, unbound generic function types such as ``<T> (T) -> T`` can be
  expressed in SIL.  Accessing a generic function with ``constant_ref`` will
  give a value of that type. The generic variables can be bound with a
  ``specialize`` instruction to give a callable function value.

Functions
---------
::

  func function_name [<T,U,V>] (T1, T2, ...) -> R {
  entry(%a1:T1, %a2:T2, ...):
    insn1
    insn2
    return
  }

A SIL function definition gives the function's name, its generic parameters (if
any), and the types of its inputs and outputs. Implicit parameters for closures
and curried functions in Swift are translated into explicit arguments.

Basic blocks
------------

The body of a function consists of one or more basic blocks. Each basic block
is introduced with a label and zero or more arguments and ends with a branch
instruction.

Instructions
------------

TODO: formalize SIL notation

Literal values
~~~~~~~~~~~~~~

constant_ref
````````````
::

  %1 = constant_ref T identifier

Loads a reference to the global object of type ``T`` represented by the
declaration ``identifier``, such as a function, method, constructor, or
property declaration. If the definition is generic, the result will be of a
generic function type; the generic variables of such a result will need to be
bound with a ``specialize`` instruction before the object can be ``apply``-ed.

zerovalue
`````````
::

  %1 = zerovalue T

Creates a "zero" value of type ``T``. This value represents the uninitialized
state of a value. This may not be a semantically valid value of type ``T``, but
will at least give predictable results.

integer_literal
```````````````
::

  %1:T = integer_literal T 123

Creates an integer literal value. The result will be of type ``T``, which must
be a builtin integer type.

float_literal
`````````````
::

  %1:T = float_literal T 1.23

Creates a floating-point literal value. The result will be of type ``T``, which
must be a builtin floating-point type.

char_literal
`````````````````
::

  %1:T = char_literal T 'x'

Creates a Unicode code point literal value. The result will be of type ``T``,
which must be of a builtin integer type.

TODO: same as integer_literal?

string_literal
``````````````
::

  %1:RawPointer = string_literal {ascii|utf8} "asdf"

Retrieves a pointer to a string literal in the string table. The result will be
of the builtin ``RawPointer`` type.

metatype
````````
::

  %1:T.metatype = metatype T

Retrieves the metatype object for type ``T``.

Memory Management
~~~~~~~~~~~~~~~~~

alloc_var
`````````
::

  %1:Address<T> = alloc_var {heap|stack} T

Allocates enough uninitialized memory to contain a value of type ``T``, either
from the heap or from the stack. The result of the instruction is the address
of the allocated memory. The memory must be deallocated with a ``dealloc``
instruction of the matching ``heap`` or ``stack`` type. The memory will not be
retainable; to allocate a retainable box for a value type, use ``alloc_box``.

alloc_ref
`````````
::

  %1:T = alloc_ref T

Allocates an object of reference type ``T``. The object will be initialized
with retain count 1; it will be uninitialized otherwise.

TODO: is this necessary, or should allocating reftypes be done by calls to
constructor functions?

alloc_tmp
`````````
TODO: does this still need to be different from alloc_var?

alloc_box
`````````
::

  %1:(ObjectPointer,Address<T>) = alloc_box T

Allocates a box large enough to hold a value of type ``T``. The result of the
instruction is a pair containing an object pointer to the box as its first
element and an address of type ``T`` pointing to the storage for the value
inside the box as its second. The box will be initialized with a retain count
of 1; the storage will be uninitialized.

alloc_array
```````````
::

  %1:(ObjectPointer,Address<T>) = alloc_array T, %0:Int

Allocates a box large enough to hold an array of ``%0`` values of type ``T``.
The result of the instruction is a pair containing an object pointer to the box
as its first element and an address of type ``T`` pointing to the storage for
the first element of the array inside the box as its second. The box will be
initialized with a retain count of 1; the storage will be uninitialized.

dealloc
```````
::

  dealloc {heap|stack} %0:Address<T>

Deallocates memory previously allocated by ``alloc_var``. The value in memory
must be released released prior to being deallocated.

retain
``````
::

  %1:T = retain %0:T

Retains the value represented by ``%0``. If it is of a value type, this is a
no-op, and ``%1`` will be equivalent to ``%0``. If the value is of a reference
type or is an object pointer, the retain count of the referenced box is
increased by one. Retaining an address is an error. The input ``%0`` is killed
by this instruction and may not be referenced after it.

release
```````
::

  release %0:T

Releases the value represented by ``%0``. If it is of a value type, this
destroys the value. If the value is of a reference type or is an object
pointer, the retain count of the referenced box is decreased by one, and if it
becomes zero, the referenced object is destroyed and the memory is deallocated.
The released value is invalid after the release instruction is executed.
Releasing an address is an error. The input ``%0`` is killed by this
instruction and may not be referenced after it.

TODO: does releasing a value type really destroy it? should destroying a value
type be a separate op?

destroy
```````
::

  destroy %0:Address<T>

Releases the value in memory at address ``%0``. This is equivalent to::

  %1 = load %0
  release %1

except that ``destroy`` must be used if ``T`` is an address-only type. This
only releases the referenced value; the memory may additionally need to be
deallocated with a separate ``dealloc`` instruction.

load
````
::

  %1:T = load [take] %0:Address<T>

Loads the value at address ``%0`` from memory. ``T`` must be a loadable type. A
load with the ``take`` attribute will destroy the value in memory (but not
deallocate the memory) in the process of the load; without the attribute, the
load will ensure the value in memory remains valid.

store
`````
::

  store %0:T -> [initialize] %1:Address<T>

Stores the value ``%0`` to memory at address ``%1``. ``T`` must be a loadable
type. A store with the ``initialize`` attribute will initialize uninitialized
memory. A store without the attribute will reassign memory with a live value
already present.

copy
````
::

  copy [take] %0:Address<T> -> [initialize] %1:Address<T>

Loads the value at address ``%0`` from memory and stores it back into memory at
address ``%1``. This is equivalent to::

  %2 = load %0
  store %2 -> %1

except that ``copy`` must be used if ``T`` is an address-only type. The
operands of ``copy`` may be given the ``take`` and ``initialize`` attributes to
indicate respectively whether the source may be destroyed and whether the
destination must be initialized.

Data manipulation
~~~~~~~~~~~~~~~~~

tuple
`````
::

  %N:(T0,T1,...) = tuple (%0:T0, %1:T1, ...)

Creates a value of a tuple type. This does not allocate any memory or retain
its inputs; those must be done explicitly in other instructions if necessary.

tuple_element
`````````````
::

  %2:TN = tuple_element %0:(T0,T1,...), %N:Int

Selects the ``%N``-th value out of a tuple or fragile struct value.

index_address
`````````````
::

  %2:Address<T> = index_address %0:Address<T>, %1:Int

Returns the address of the ``%1``-th element relative to ``%0``.

TODO: could it also index into tuples and structs and into multilevel
structures like GEP?

convert
```````
::

  %1:U = convert %0:T -> U

Performs an implicit conversion from ``T`` to ``U``. This instruction is
limited to conversions that will not affect how the value will codegen.

TODO: what does that mean in practical terms?

Functions
~~~~~~~~~

closure
```````
::

  %C:((TN+1,...) -> R) = closure %0:((T1,...) -> R), (%1:T1, %2:T2, ..., %N:TN)

Allocates a closure by partially applying the function ``%0`` in its first
``N`` arguments. The closure will be a allocated as a box with retain count 1
containing the values ``%1`` through ``%N``, which must be retained if
necessary in separate instructions.

specialize
``````````
::

  %1:F1 = specialize %0:F0 -> F1

Specializes a generic function of generic function type ``F0`` to generic or
concrete function type ``F1``, binding some or all of its generic type
variables.

apply
`````
::

  %R:R = apply %0:((T1, T2, ...) -> R) (%1:T1, %2:T2, ...)

Transfers control to function ``%0``, passing in the given arguments. The
``apply`` instruction does no retaining or releasing of its arguments by
itself; the calling convention's retain/release policy must be handled by
separate explicit ``retain`` and ``release`` instructions. The return value
will likewise not be implicitly retained or released. ``%0`` must be an object
of a concrete function type; generic functions must have all of their generic
parameters bound with ``specialize`` instructions before they can be applied.

TODO: should have normal/unwind branch targets like LLVM ``invoke``

Branching
~~~~~~~~~

Branching instructions terminate a basic block.

unreachable
```````````
::

  unreachable

Instruction indicates that control flow must not reach the end of the current
basic block.

return
``````
::

  return %0:T

Exits the current function and returns control to the calling function. The
result of the ``apply`` instruction that invoked the current function will be
the operand of this ``return`` instruction.  ``return`` does not retain or
release its operand or any other values.

branch
``````
::

  branch label (%1:T1, %2:T2, ...)

Unconditionally transfers control from the current basic block to the block
labeled ``label``, passing the given values as arguments to ``label``.

cond_branch
```````````
::

  cond_branch %0:Int1, true_label (%T1:T1, %T2:T2, ...),
                       false_label (%F1:F1, %F2:F2, ...)

Conditionally branches to ``true_label`` if ``%0`` is equal to one or to
``false_label`` if ``%0`` is equal to zero, passing the corresponding set of
values as arguments to the chosen block. ``%0`` must be of the builtin ``Int1``
type.

TODO: throw

Examples
--------

TODO
