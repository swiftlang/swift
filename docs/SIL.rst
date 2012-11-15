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
function takes the function's arguments as its own. For example, the following
Swift code::

  func fizzbuzz(fizz:Bool, buzz:Bool) {
    var s:String
    if fizz && buzz {
      s = "FizzBuzz"
    } else if fizz {
      s = "Fizz"
    } else {
      s = "Buzz"
    }
    println(s)
  }

might get optimized down to the following SIL::

  func @fizzbuzz : $(Bool, Bool) -> () {
  entry(%fizz:$Bool, %buzz:$Bool):
    %fizzandbuzz = apply @Builtin.and(%fizz, %buzz)
    cond_branch %fizzandbuzz, fizzandbuzz(), notfizzandbuzz()

  fizzandbuzz():
    %s1 = string_literal ascii "FizzBuzz"
    branch print(%s1)

  notfizzandbuzz():
    cond_branch %fizz, fizz(), buzz()

  fizz():
    %s2 = string_literal ascii "Fizz"
    branch print(%s2)

  buzz():
    %s3 = string_literal ascii "Buzz"
    branch print(%s3)

  print(%s:RawPointer):
    %string = apply @convertFromStringLiteral<String>(%s)
    %void = apply @println(%string)
  }

In Swift, memory management is almost always implicit, but in SIL, it is always
explicit. Allocation, deallocation, and reference counting have explicit
instructions in SIL, and instructions such as tuple construction and function
calls in SIL never implicitly retain or release objects even if the analogous
high-level operations in Swift do.

Values
------

SIL notation uses a scheme similar to LLVM assembly language, where local
values are preceded by a ``%`` and global names are preceded by an ``@``.
Type names are additionally preceded by a ``$``. Global names follow Swift
dotted-name parsing rules, and type names follow Swift type parsing rules.
Local ``%`` value names may consist of an integer ID or a Swift identifier.

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

* The *address of T* ``$SIL.Address<T>``, a pointer to memory containing a
  value of any reference or value type ``$T``.  This can be an internal pointer
  into a data structure. Addresses of loadable types can be loaded and stored
  to access values of those types. Addresses of address-only types can only be
  used with the ``copy``, ``destroy``, and ``dealloc`` instructions, or as
  arguments to functions. Addresses cannot be retained or released.
* The *object pointer* ``$SIL.ObjectPointer`` is a generic pointer to a
  retainable block of memory, or *box*. An object pointer behaves like a
  reference type, but its contents are not accessible; it can only be retained,
  released, or passed around. Operations that allocate retainable memory
  generally return both an object pointer for the box and an address pointing
  to the object or objects inside the box.
* Unlike Swift, unbound generic function types such as ``$<T> (T) -> T`` can be
  expressed in SIL.  Accessing a generic function with ``constant_ref`` will
  give a value of that type. The generic variables can be bound with a
  ``specialize`` instruction to give a callable function value.

Functions
---------
::

  func @function_name : $<T,U,V> (A1, A2, ...) -> R {
  entry(%a1:$A1, %a2:$A2, ...):
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

In the instruction descriptions, ``[optional attributes]`` appear in square 
brackets, and ``{required|attribute|choices}`` appear in curly braces with
options separated by pipes. Variadic operands are indicated with ``...``.

Literal values
~~~~~~~~~~~~~~

constant_ref
````````````
::

  %1 = constant_ref $T @global
  ; %1 has type $T

Loads a reference to the global object of type ``T`` represented by the
declaration ``identifier``, such as a function, method, constructor, or
property declaration. If the definition is generic, the result will be of a
generic function type; the generic variables of such a result will need to be
bound with a ``specialize`` instruction before the object can be ``apply``-ed.

zero_value
``````````
::

  %1 = zero_value $T
  ; %1 has type $T

Creates a "zero" value of type ``T``. This value represents the uninitialized
state, so it may not be a semantically valid value of type ``T``, but
will at least give predictable results.

integer_literal
```````````````
::

  %1 = integer_literal $T 123
  ; $T must be a builtin integer type
  ; %1 has type $T

Creates an integer literal value. The result will be of type ``T``, which must
be a builtin integer type.

float_literal
`````````````
::

  %1 = float_literal $T 1.23
  ; $T must be a builtin floating-point type
  ; %1 has type $T

Creates a floating-point literal value. The result will be of type ``T``, which
must be a builtin floating-point type.

char_literal
````````````
::

  %1 = char_literal $T 'x'
  ; $T must be a builtin integer type
  ; %1 has type $T

Creates a Unicode code point literal value. The result will be of type ``T``,
which must be of a builtin integer type.

TODO: same as integer_literal?

string_literal
``````````````
::

  %1 = string_literal {ascii|utf8} "asdf"
  ; %1 has type $Builtin.RawPointer

Retrieves a pointer to a string literal in the string table. The result will be
of the builtin ``RawPointer`` type.

metatype
````````
::

  %1 = metatype $T
  ; $T must be a type
  ; %1 has type $T.metatype

Retrieves the metatype object for type ``T``.

Memory Management
~~~~~~~~~~~~~~~~~

alloc_var
`````````
::

  %1 = alloc_var {heap|stack|pseudo} $T
  ; $T must be a type
  ; %1 has type $SIL.Address<T>

Allocates enough uninitialized memory to contain a value of type ``T``, either
from the heap or from the stack. The result of the instruction is the address
of the allocated memory. The memory must be deallocated with a ``dealloc``
instruction of the matching ``heap`` or ``stack`` type. The memory will not be
retainable; to allocate a retainable box for a value type, use ``alloc_box``.

An ``alloc_var`` may also perform a ``pseudo`` allocation, which is a stack
allocation for debugging or tooling purposes. A pseudo-allocation does not
need to be deallocated or destroyed and should only be stored to by the program.

alloc_ref
`````````
::

  %1 = alloc_ref $T
  ; $T must be a type
  ; %1 has type $T

Allocates an object of reference type ``T``. The object will be initialized
with retain count 1; it will be uninitialized otherwise.

TODO: is this necessary, or should allocating reftypes be done by calls to
constructor functions?

alloc_box
`````````
::

  %1 = alloc_box $T
  ; $T must be a type
  ; %1 has type $(SIL.ObjectPointer, SIL.Address<T>)

Allocates a box large enough to hold a value of type ``T``. The result of the
instruction is a pair containing an object pointer to the box as its first
element and an address of type ``T`` pointing to the storage for the value
inside the box as its second. The box will be initialized with a retain count
of 1; the storage will be uninitialized.

alloc_array
```````````
::

  %1 = alloc_array $T, %0
  ; $T must be a type
  ; %0 must be of a builtin integer type
  ; %1 has type $(SIL.ObjectPointer,SIL.Address<T>)

Allocates a box large enough to hold an array of ``%0`` values of type ``T``.
The result of the instruction is a pair containing an object pointer to the box
as its first element and an address of type ``T`` pointing to the storage for
the first element of the array inside the box as its second. The box will be
initialized with a retain count of 1; the storage will be uninitialized.

dealloc
```````
::

  dealloc {heap|stack} %0
  ; %0 must be of a $SIL.Address<T> type

Deallocates memory previously allocated by ``alloc_var``. The value in memory
must be released released prior to being deallocated.

retain
``````
::

  retain %0

Retains the value represented by ``%0``. If it is of a value type, this is a
no-op. If the value is of a reference type or is an object pointer, the retain
count of the referenced box is increased by one. Retaining an address is an
error.

release
```````
::

  release %0

Releases the value represented by ``%0``. If it is of a value type, this
destroys the value. If the value is of a reference type or is an object
pointer, the retain count of the referenced box is decreased by one, and if it
becomes zero, the referenced object is destroyed and the memory is deallocated.
Releasing an address is an error.

TODO: does releasing a value type really destroy it? should destroying a value
type be a separate insn?

destroy
```````
::

  destroy %0
  ; %0 must be of a $SIL.Address<T> type

Releases the value in memory at address ``%0``. This is equivalent to::

  %1 = load %0
  release %1

except that ``destroy`` must be used if ``%0`` is of an address-only type. This
only releases the referenced value; the memory may additionally need to be
deallocated with a separate ``dealloc`` or ``release`` instruction.

load
````
::

  %1 = load [take] %0
  ; %0 must be of a $SIL.Address<T> type for a loadable type $T
  ; %1 will be of type $T

Loads the value at address ``%0`` from memory. ``T`` must be a loadable type. A
load with the ``take`` attribute will destroy the value in memory (but not
deallocate the memory) in the process of the load; without the attribute, the
load will ensure the value in memory remains valid.

store
`````
::

  store %0 -> [initialize] %1
  ; Given a %0 of loadable type $T,
  ; %1 must be of type $SIL.Address<T>

Stores the value ``%0`` to memory at address ``%1``. ``%0`` must be of a
loadable type. A store with the ``initialize`` attribute will initialize
uninitialized memory. A store without the attribute will reassign memory with a
live value already present.

copy
````
::

  copy [take] %0 -> [initialize] %1
  ; %0 and %1 must be of the same $SIL.Address<T> type

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

  %N = tuple (%0, %1, ...)
  ; Given %0 of type $T0, %1 of type $T1, etc.,
  ; %N will be of type $(T0, T1, ...)

Creates a value of a tuple type with zero or more elements. This does not
allocate any memory or retain any inputs.

tuple_element
`````````````
::

  %1 = tuple_element %0, 123
  ; %0 must be of a tuple type $(T0, T1, ...) or a fragile struct type
  ; %1 will be the type of the 123rd element

Selects a value out of a tuple or fragile struct value.

index_address
`````````````
::

  %2 = index_address %0, %1
  ; %0 must be of a $SIL.Address<T> type
  ; %1 must be of a builtin integer type
  ; %2 will be of the same $SIL.Address<T> type as %0

Returns the address of the ``%1``-th element relative to ``%0``.

convert
```````
::

  %1 = convert %0 -> $T
  ; $T must be a type
  ; %0 must be of a type $U implicitly convertible to $T
  ; %1 will be of type $T

Performs an implicit conversion of ``%0`` to type ``T``. This instruction is
limited to conversions that will not affect how the value will codegen.

TODO: what exactly is implicitly convertible at the sil level?

Functions
~~~~~~~~~

closure
```````
::

  %C = closure %0(%1, %2, ...)
  ; %0 must be of a function type $(A1, A2, ...) -> R
  ; %1, %2, etc. must be of the types of the first N arguments to %0
  ; %C will be of the function type of %0 with the first N arguments removed

Allocates a closure by partially applying the function ``%0`` in its first
N arguments. The closure will be a allocated as a box with retain count 1
containing the values ``%1``, ``%2``, etc. The closed-over values will not be
retained; that must be done separately if necessary.

specialize
``````````
::

  %1 = specialize %0 -> $T
  ; %0 must be of a generic function type $<T1, T2, ...> A -> R
  ; $T must be of either the concrete function type $A -> R or a generic
  ; function type $<T3, ...> A -> R with some type variables removed.
  ; %1 will be of the function type $T

Specializes a generic function ``%0`` to the generic or concrete function type
``T``, binding some or all of its generic type variables.

apply
`````
::

  %R = apply %0(%1, %2, ...)
  ; %0 must be of a concrete function type $(A1, A2, ...) -> R
  ; %1, %2, etc. must be of the argument types $A1, $A2, etc.
  ; %R will be of the return type $R

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

  return %0
  ; %0 must be of the return type of the current function

Exits the current function and returns control to the calling function. The
result of the ``apply`` instruction that invoked the current function will be
the operand of this ``return`` instruction.  ``return`` does not retain or
release its operand or any other values.

branch
``````
::

  branch label (%0, %1, ...)
  ; `label` must refer to a block label within the current function
  ; %0, %1, etc. must be of the types of `label`'s arguments

Unconditionally transfers control from the current basic block to the block
labeled ``label``, passing the given values as arguments to ``label``.

cond_branch
```````````
::

  cond_branch %0, true_label (%T1, %T2, ...),
                  false_label (%F1, %F2, ...)
  ; %0 must be of the builtin Int1 type
  ; `true_label` and `false_label` must refer to block labels within the
  ;   current function
  ; %T1, %T2, etc. must be of the types of `true_label`'s arguments
  ; %F1, %F2, etc. must be of the types of `false_label`'s arguments

Conditionally branches to ``true_label`` if ``%0`` is equal to one or to
``false_label`` if ``%0`` is equal to zero, passing the corresponding set of
values as arguments to the chosen block. ``%0`` must be of the builtin ``Int1``
type.

TODO: throw

Examples
--------

Trivial example
~~~~~~~~~~~~~~~

A simple Swift function::

  func foo(b:Int) {
    var a = b
    f(a)
  }

will be emitted as the following SIL::

  ; decl "func foo"
  func @foo: $(Int) -> () {
  entry(%b:$Int):
    ; prologue
    %b_alloc = alloc_box $Int
    %b_addr = tuple_element %b_alloc, 1
    store %b -> initialize %b_addr

    ; decl "var a"
    %a_alloc = alloc_box $Int
    %a_addr = tuple_element %a_alloc, 1
    ; expression "b"
    %1 = load %b_addr
    ; initializer "var a = b"
    store %1 -> initialize %a_addr

    ; expression "a"
    %2 = load %a

    ; expression "f"
    %3 = constant_ref $(Int) -> Int @f

    ; expression "f(a)"
    retain %2 ; parameters are passed at +1
    %4 = apply %3(%2)
    ; cleanup for full expr "f(a)"
    release %4 ; return values returned at +1

    ; cleanup for block
    %a_box = tuple_element %a_alloc, 0
    release %a_box

    ; epilogue
    %b_box = tuple_element %b_alloc, 0
    release %b_box ; arguments are received at +1
    %void = tuple ()
    return %void
  }

Note that all the memory management and allocation implicit to the Swift code
is made explicit in the SIL codegen. Optimization will simplify that into this::

  func @foo: $(Int) -> () {
  entry(%b:Int):
    %b_dbg = alloc_var pseudo $Int
    store %b -> initialize %b_dbg

    %a_dbg = alloc_var pseudo $Int
    store %b -> initialize %a_dbg

    %1 = apply @f(%b)
    release %1 ; if return value of ``f`` is not POD

    %void = tuple ()
    return %void
  }

Escape analysis detects that the boxes allocated for ``a`` and ``b``
are unnecessary and eliminates the boxes, replacing them with a ``pseudo``
allocation for debugging purposes. The variables are also reduced to registers.
Since ``Int`` is a POD type, the retain and release operations are no-ops and
are also eliminated.

Closures
~~~~~~~~

A function that closes over a local argument and lets the closure escape::

  func adder(x:Int) -> (y:Int) -> Int {
    return func(y) { x + y }
  }

will be emitted as SIL::

  ; decl "func adder"
  func @adder: $(Int) -> (Int) -> Int {
  entry(%x:Int):
    ; prologue
    %x_alloc = alloc_box $Int
    %x_addr = tuple_element %x_alloc, 1
    store %x -> initialize %x_addr

    ; expression "func(y)..."
    %1 = constant_ref $(SIL.ObjectPointer, SIL.Address<Int>, Int) -> Int \
                      @adder_1
    %x_box = tuple_element %x_alloc, 0
    retain %x_box
    %2 = closure %1(%x_box, %x_addr)

    ; epilogue
    release %x_box
    return %2
  }

  ; decl for anonymous function
  func @adder_1: $(SIL.ObjectPointer, SIL.Address<Int>, Int) -> Int {
  entry(%x_box:SIL.ObjectPointer, %x_addr:SIL.Address<Int>, %y:Int):
    ; prologue
    %y_alloc = alloc_box $Int
    %y_addr = tuple_element %y_alloc, 1
    store %y -> initialize %y_addr

    ; expression "x"
    %1 = load %x_addr
    ; expression "y"
    %2 = load %y_addr
    ; expression "+"
    %3 = constant_ref $(Int, Int) -> Int @+
    ; expression "x + y"
    %4 = apply %3(%1, %2)

    ; epilogue
    %y_box = tuple_element %y_alloc, 0
    release %y_box
    return %4
  }

FIXME: can we avoid closing over two values for a mutable capture?

That the closed-over variable is represented as a pair of parameters to
the closure, the box holding the variable's reference count and the address
of the variable inside the box. The outer function retains the box explicitly
before embedding it in the closure with a ``closure`` instruction. In this case,
the variable ``x`` is not modified, so optimization can reduce the capture to
a primitive value::

  func @adder: $(Int) -> (Int) -> Int {
  entry(%x:Int):
    %x_dbg = alloc_var pseudo $Int
    store %x -> initialize %x_dbg
    %1 = closure @adder_1(%x)
    return %1
  }

  func @adder_1: $(Int, Int) -> Int {
  entry(%x:Int, %y:Int):
    %x_dbg = alloc_var pseudo $Int
    store %x -> initialize %x_dbg
    %y_dbg = alloc_var pseudo $Int
    store %y -> initialize %y_dbg
    %1 = apply @+(%1, %2)
    return %1
  }
