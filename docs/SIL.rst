Swift Intermediate Language
===========================

.. contents::

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
explicit. Allocation, deallocation, destruction, and reference counting have
explicit instructions in SIL, and instructions such as aggregate construction
and function calls in SIL never implicitly retain or release objects even if
the analogous high-level operations in Swift do.

Notation
--------

SIL notation uses a scheme similar to LLVM assembly language, in which program
identifiers are perfixed with sigils and bare keywords are reserved for IR
syntax. Comments are introduced with ``;`` and go to the end of the line::

  ; This is a comment
  This isn't

Operand names are preceded by a ``%``. An operand may represent multiple
values, in which case a value must be selected with ``#`` followed by an
integer. Operand names may consist of an integer or Swift dotted name::

  ; These are operands:
  %0
  %1
  %zero
  %one
  %a.b.c

  ; These are multiple-value operands:
  %multi#0
  %multi#1

Global names are preceded by an ``@`` and follow Swift dotted-name parsing
rules. Specialized instances of generic names may be referenced by putting
the generic parameters in angle brackets::

  ; These are globals:
  @abort
  @exit
  @Builtin.add
  @Builtin.add<Builtin.Int64>

Type names are preceded by a ``$``  and follow Swift type parsing rules::

  ; These are types:
  $Int
  $Builtin.Int64
  $Slice<Int>
  $Int[]
  $(Int, Int)
  $(Int, Int) -> Int

Some instructions take integer, floating-point, or string literals as
operands; these follow the same parsing rules as literals in Swift.

Operands
--------

Most instructions take only local ``%`` operands. Special instructions
are needed to load a local operand value referencing a global constant or
literal value, for example ``constant_ref`` for globals or ``integer_literal`` 
for integers.

Types
-----

SIL's type system is Swift's with some additional aspects. Like Swift, there
are two broad categories of types based on value semantics:

* *reference types*, which are handles to reference-counted boxes and are
  stored and passed around by reference, for example, classes and functions.
* *value types*, which are stored in-line and passed by value, for example,
  structs, tuples, and primitive types.

SIL classifies types into two additional subgroups based on ABI stability:

* *Loadable types* are types with a fully exposed concrete representation:

  * Reference types,
  * Builtin value types,
  * Fragile struct types in which all element types are loadable, and
  * Tuple types in which all element types are loadable.

  A *loadable aggregate type* is a tuple or struct type that is loadable.

* *Address-only types* are value types for which the compiler cannot access a
  full concrete representation:
  
  * Resilient value types,
  * Fragile struct or tuple types that contain resilient types as elements at
    any depth,
  * Protocol types, and
  * Generic archetypes.

  Values of address-only types must reside in memory and can only be referenced
  in SIL by address. Address-only type addresses cannot be loaded from or
  stored to. SIL provides special instructions for indirectly accessing
  address-only values.

SIL adds some additional types of its own, which are not first-class Swift
types but are needed for some operations:

* The *address of T* ``$*T``, a pointer to memory containing a
  value of any reference or value type ``$T``.  This can be an internal pointer
  into a data structure. Addresses of loadable types can be loaded and stored
  to access values of those types.
  Addresses of address-only types can only be used with instructions that
  manipulate their operands indirectly by address, such as ``copy_addr``,
  ``destroy_addr``, and ``dealloc_var``, or as arguments to functions.
  Addresses cannot be retained or released.
* The primitive ``$Builtin.ObjectPointer`` type is used to represent the
  *box*, a typeless reference to a reference-counted block
  of memory. A box can be either an instance of a reference type or a
  reference-counted storage area for a value type. The contents of a box are not
  accessible through the object pointer; boxes can only be retained, released,
  or passed around as opaque operands. Operations that allocate retainable
  memory generally return both a box and a typed address pointing
  into the box.
* Unlike Swift, values of unbound *generic function types* such as
  ``$<T...> (A...) -> R`` can be expressed in SIL.  Accessing a generic
  function with ``constant_ref`` will give a value of a generic function type.
  Its type variables can be bound with a ``specialize`` instruction to
  give a value of a *concrete function type* ``$(A...) -> R`` that can then
  be ``apply``-ed.

Swift types may not translate one-to-one to SIL types. In particular, tuple
types are canonicalized, and function types are canonicalized and mangled in
order to encode calling convention and resilience rules. Loadable struct types
are assigned an ordering for their fields which is used to numerically index
the fields in aggregate manipulation instructions.

Functions
---------
::

  func @function_name : $<T,U,V> (A1, A2, ...) -> R {
  entry(%a1:$A1, %a2:$A2, ...):
    insn1
    insn2
    return
  }

A SIL function definition gives the function's name as a global symbol, its
generic parameters (if any), and the types of its inputs and outputs. Implicit
parameters for closures and curried functions in Swift are translated into
explicit arguments.

Basic blocks
------------

The body of a function consists of one or more basic blocks. Each basic block
is introduced with a label name followed by zero or more arguments and ends
with a branch instruction. Label names are local to the function body.

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

  %1 = constant_ref $T, @global
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

FIXME: this is a stopgap that will be eliminated when we have dataflow passes
to prevent uninitialized access

Creates a "zero" value of type ``T``. This value represents the uninitialized
state of a variable, so it may not be a semantically valid value of type ``T``.

zero_addr
`````````
::

  zero_addr %0
  ; %0 must be the address of an address-only type

FIXME: this is a stopgap that will be eliminated when we have dataflow passes
to prevent uninitialized access

Zero-initializes the memory referenced by ``%0``. This is similar to::

  %1 = zero_value $T
  store %1 to %0

but ``zero_addr`` must be used if ``$T`` is an address-only type.

integer_literal
```````````````
::

  %1 = integer_literal $T, 123
  ; $T must be a builtin integer type
  ; %1 has type $T

Creates an integer literal value. The result will be of type ``T``, which must
be a builtin integer type.

float_literal
`````````````
::

  %1 = float_literal $T, 1.23
  ; $T must be a builtin floating-point type
  ; %1 has type $T

Creates a floating-point literal value. The result will be of type ``T``, which
must be a builtin floating-point type.

string_literal
``````````````
::

  %1 = string_literal {ascii|utf8} "asdf"
  ; %1 has type $(Builtin.RawPointer, Builtin.Int64)

Retrieves a pointer to a string literal in the string table. The result will
be a pair, the first element of which is a ``Builtin.RawPointer`` pointing to
the first byte of a zero-terminated string in the specified ``ascii`` or
``utf8`` encoding, and the second element of which is a ``Builtin.Int64`` value
representing the size in bytes of the encoded string.

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
  ; %1 has type $*T

Allocates enough uninitialized memory to contain a value of type ``T``, either
from the heap or from the stack. The result of the instruction is the address
of the allocated memory. The memory must be deallocated with a ``dealloc_var``
instruction of the matching ``heap`` or ``stack`` type. The memory will not be
retainable; to allocate a retainable box for a value type, use ``alloc_box``.

An ``alloc_var`` may also perform a ``pseudo`` allocation, which is a stack
allocation for debugging or tooling purposes. A pseudo-allocation does not
need to be deallocated or destroyed and should only be stored to by the program.

alloc_ref
`````````
::

  %1 = alloc_ref {heap|stack} $T
  ; $T must be a reference type
  ; %1 has type $T

Allocates an object of reference type ``T``. The object will be initialized
with retain count 1; its state will be otherwise uninitialized. The object
may be allocated on the heap or stack; although reference types are normally
heap-allocated and released with a ``release`` instruction, optimization
may lower the allocation to a stack allocation and the release to a
``dealloc_ref``.

alloc_box
`````````
::

  %1 = alloc_box {heap|stack} $T1, $T2, ..., $TN
  ; %1 is N+1 values:
  ;   %1#0 has type SIL.Box
  ;   %1#1 has type *T1
  ;   %1#2 has type *T2
  ;               ⋮
  ;   %1#N has type *TN
  ; TODO: alloc_box is only implemented for a single type argument

Allocates a box large enough to hold ``N`` values of types ``T1`` through
``TN``. The result of the instruction is a multiple-value operand consisting of
an object pointer to the box as its first element followed by addresses of type
``*T1`` through ``*TN`` pointing into the
storage for the values inside the box. The box will be initialized
with a retain count of 1; the storage will be uninitialized and must
be initialized with ``store`` instructions before the address can be
``load``-ed or the box can be ``release``-d. When the box's retain count
reaches zero, the values inside the box will all be ``release``-d if necessary.
Boxes are normally heap-allocated and released with a ``release`` instruction,
but optimization may lower the allocation to a stack allocation and the
release to a ``dealloc_ref``.

alloc_array
```````````
::

  %1 = alloc_array $T, %0
  ; $T must be a type
  ; %0 must be of a builtin integer type
  ; %1 has type $(SIL.Box,*T)

Allocates a box large enough to hold an array of ``%0`` values of type ``T``.
The result of the instruction is a pair containing an object pointer to the box
as its first element and an address of type ``T`` pointing to the storage for
the first element of the array inside the box as its second. The box will be
initialized with a retain count of 1; the storage will be uninitialized. The
storage must be initialized before the address can be ``load``-ed or the box
can be ``release``-d. When the box's retain count reaches zero, the values
inside the box will be ``release``-d.

dealloc_var
```````````
::

  dealloc_var {heap|stack} %0
  ; %0 must be of a $*T type

Deallocates memory previously allocated by ``alloc_var``. The value in memory
must be destroyed prior to being deallocated, and the ``heap`` or ``stack``
attribute must match the corresponding ``alloc_var`` instruction.

dealloc_ref
```````````
::

  dealloc_ref {heap|stack} %0
  ; %0 must be of a box or reference type

Deallocates a box or reference type instance. The box must have a
retain count of one, and the ``heap`` or ``stack`` attribute must match the
corresponding ``alloc_box`` or ``alloc_ref`` instruction. This does not
destroy the reference type instance or the values inside the box; this must
be done manually by ``release``-ing any releasable values inside the
value and calling its destructor function before the value is deallocated.

retain
``````
::

  retain %0
  ; %0 must be of a box or reference type

Retains the box or reference type instance represented by ``%0``. Retaining
an address or value type is an error.

release
```````
::

  release %0
  ; %0 must be of a box or reference type

Releases the box or reference type represented by ``%0``. If the release
operation brings the retain count of the value to zero, the referenced object
is destroyed and its memory is deallocated. A stack-allocated box must not
be released to reference count zero; it must instead be destroyed manually and
then deallocated with a ``dealloc_ref stack`` instruction. Releasing an
address or value type is an error.

destroy_addr
````````````
::

  destroy_addr %0
  ; %0 must be of a $*T type

Destroys the value in memory at address ``%0``. This is equivalent to::

  %1 = load %0
  release %1

except that ``destroy_addr`` must be used if ``%0`` is of an address-only type.
This only destroys the referenced value; the memory may additionally need to be
deallocated with a separate ``dealloc_var`` instruction.

load
````
::

  %1 = load %0
  ; %0 must be of a $*T type for a loadable type $T
  ; %1 will be of type $T

Loads the value at address ``%0`` from memory. ``T`` must be a loadable type.
This does not affect the reference count, if any, of the loaded value; the
value must be retained explicitly if necessary.

store
`````
::

  store %0 to %1
  ; Given a %0 of loadable type $T,
  ; %1 must be of type $*T

Stores the value ``%0`` to memory at address ``%1``. ``%0`` must be of a
loadable type. This will overwrite the memory at ``%1``; any existing value at
``%1`` must be released or destroyed before being overwritten.

copy_addr
`````````
::

  copy_addr [take] %0 to [assign] %1
  ; %0 and %1 must be of the same $*T type

Loads the value at address ``%0`` from memory and stores it back into memory at
address ``%1``. A bare ``copy_addr`` instruction::

  copy_addr %0 to %1

is equivalent to::

  %tmp = load %0
  retain %tmp ; if %tmp is of a box or reference type
  store %tmp to %1

except that ``copy`` must be used if ``%0`` is of an address-only type. The
operands of ``copy`` may be given one or both of the ``take`` or ``assign``
attributes:

* ``take`` indicates that ownership of resources may be taken from the source
  value at ``%0`` and given to ``%1``, invalidating ``%0``. Without ``take``,
  ``copy_addr`` will retain resources in ``%0`` so that both ``%0`` and ``%1``
  are valid after the instruction.
* ``assign`` indicates that ``%1`` already contains a valid value which must be
  ``release``-d before being replaced with the value at ``%0``. Without
  ``assign``, ``copy_addr`` will overwrite the memory at ``%1`` as if it is
  uninitialized.

The three attributed forms thus behave like the following loadable type
operations::

  ;;; take-initialization
    copy_addr take %0 to %1
  ;;; is equivalent to:
    %tmp = load %0
    ; no retain!
    store %tmp to %1

  ;;; assignment
    copy_addr %0 to assign %1
  ;;; is equivalent to:
    %tmp_src = load %0
    retain %tmp_src
    %tmp_dest = load %1
    store %tmp_src to %1
    release %tmp_dest

  ;;; take-assignment
    copy_addr take %0 to assign %1
  ;;; is equivalent to:
    %tmp_src = load %0
    ; no retain %tmp_src!
    %tmp_dest = load %1
    store %tmp_src to %1
    release %tmp_dest

Data manipulation
~~~~~~~~~~~~~~~~~

construct
`````````
::

  %N = construct $T, (%0, %1, ...)
  ; $T must be a loadable aggregate type
  ; %0, %1, etc. must be of the types of the fields of $T in order
  ; %N will be of type $T
  ; TODO: not implemented

Creates a value of a loadable aggregate type with zero or more elements.
This does not allocate any memory or retain any inputs.

extract
```````
::

  %1 = extract %0, 123
  ; %0 must be of a loadable aggregate type
  ; %1 will be of the type of the 123rd element of %0

Extracts an element of a loadable aggregate value.

insert
``````
::

  %2 = insert %0, 123, %1
  ; %0 must be of a loadable aggregate type
  ; %1 must be of the type of the 123rd element of %0
  ; %2 will be of the same type as %0
  ; TODO: not implemented

Create a new value of a loadable aggregate value equal to another value of
that type with a single element replaced.

element_addr
````````````
::

  %1 = element_addr %0, 123
  ; %0 must of a $*T type for a loadable aggregate type T
  ; %1 will be of type $*U where U is the type of the 123rd
  ;   element of T

Given the address of a loadable aggregate value in memory, creates a
value representing the address of an element within that value.

ref_element_addr
````````````````
::

  %1 = ref_element_addr %0, 123
  ; %0 must be of a reference type $T
  ; %1 will be of type $*U where U is the type of the 123rd
  ;   element of T

Given a value of a reference type, creates a value representing the address
of an element within the referenced instance.

index_addr
``````````
::

  %2 = index_addr %0, %1
  ; %0 must be of a $*T type
  ; %1 must be of a builtin integer type
  ; %2 will be of the same $*T type as %0

Given a pointer into an array of values, returns the address of the
``%1``-th element relative to ``%0``.

implicit_convert
````````````````
::

  %1 = implicit_convert %0, $T
  ; %0 must be of a type $U implicitly convertible to $T (see below)
  ; %1 will be of type $T

Performs an implicit conversion of ``%0`` to type ``T``. This instruction is
limited to conversions that have no runtime effect, such as:

* derived-to-base conversion
* scalar-to-equivalent-tuple conversion
* function-to-equivalent-function conversion
* reference-type-to-``Box`` conversion

downcast
````````
::

  %1 = downcast %0, $T
  ; %0 must be of a reference type that is a subclass of $T
  ; $T must be a class type
  ; %1 will be of type T

Performs a checked downcast conversion of ``%0`` to subclass ``T``.

FIXME: if it fails...

coerce
``````
::

  %1 = coerce %0, $T
  ; %0 must be of type $T
  ; %1 will be of type $T

Represents an explicit type coercion with no runtime effect. ``%1`` will be
equivalent to ``%0``.

Generics
~~~~~~~~

specialize
``````````
::

  %1 = specialize %0, $T
  ; %0 must be of a generic function type $<T1, T2, ...> A -> R
  ; $T must be of either the concrete function type $A -> R or a generic
  ; function type $<T3, ...> A -> R with some type variables removed.
  ; %1 will be of the function type $T

Specializes a generic function ``%0`` to the generic or concrete function type
``T``, binding some or all of its generic type variables.

generalize
``````````
::

  %1 = generalize %0, $T
  ; $T must be a generic type
  ; %1 will be of type $T
  ; TODO: not implemented

Performs a representation conversion of ``%0`` to type ``T``, which must be a
generic type compatible with the type of ``%0``.

archetype_to_super
``````````````````
::

  %1 = archetype_to_super %0, $T
  ; %0 must be an address of an archetype $*U with base class constraint U : B
  ; $T must be the base constraint type B or a superclass of B
  ; %1 will be of the base type $T

Performs an upcast operation on the archetype value referenced by ``%0``.

super_to_archetype
``````````````````
::

  super_to_archetype %0 to %1
  ; %0 must be of a reference type $T
  ; %1 must be the address of an archetype $*U with base class constraint U : B
  ;   where B is T or a subclass of T

Performs a checked downcast operation on the class instance referenced by
``%0``, initializing the archetype referenced by ``%1`` with a reference to
the class instance if the check succeeds.

FIXME: if it fails...

archetype_method
````````````````
::

  %1 = archetype_method %0, @method
  ; %0 must be an address of an archetype $*T
  ; @method must be a reference to a method of one of the constraints of T
  ; %1 will be of type T -> U' -> V' for method type U -> V,
  ;   where self and associated types in U and V are bound relative to T in
  ;   U' and V'
  ;   e.g. method `(This, Foo) -> Protocol.Bar` becomes `(T, Foo) -> T.Bar`

Obtains a reference to function implementing ``@method`` for the archetype
referenced by ``%0``. Self and associated types in the signature of ``@method``
are bound relative to the type referenced by ``%0`` in the resulting function
value.

associated_metatype
```````````````````
::

  %1 = associated_metatype %0, $U
  ; %0 must be a metatype value of type $T.metatype
  ; $U must be an associated type of $T

Obtains the metatype object for the associated type ``$U`` of the type with
metatype ``%0``.

Protocol and protocol composition types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

From SIL's perspective, protocol and protocol composition types consist of 
an *existential container*, which gets allocated when
``alloc_var`` or ``alloc_box`` is applied to a protocol or protocol composition
type. An existential container is a generic container for
a value of unknown runtime type, referred to as an "existential type" in
type theory. The existential container consists of a reference to the *witness
table(s)* for the protocol(s) referred to by the protocol type and a reference
to the underlying *concrete value*, which may be either stored in-line inside
the existential container for small values or allocated separately into a
buffer owned and managed by the existential container for larger values.

Existential containers are always address-only. The value semantics of
the existential container propagate to the contained concrete value. Applying
``copy_addr`` to an existential container copies the
contained concrete value, deallocating or reallocating the destination's
owned buffer if necessary. Applying ``destroy_addr`` to an existential
container destroys the concrete value and deallocates any buffers owned by
the existential container.

An existential container's witness tables and concrete value buffer
are prepared by applying the ``init_existential`` instruction to an
uninitialized existential container. ``init_existential`` takes a
concrete type parameter and returns an address of the given type that can then
be stored to in order to fully initialize the existential container.
For example, creating a protocol value from a value type in Swift::

  protocol SomeProtocol
  struct SomeInstance : SomeProtocol

  var x:SomeInstance
  var p:SomeProtocol = x

compiles to this SIL::

  ; allocate the existential container for a SomeProtocol
  %p = alloc_var $SomeProtocol
  ; initialize the existential container to contain a SomeInstance
  %p_instance = init_existential $SomeInstance, %p
  ; store the SomeInstance inside the existential container
  store %x to %p_instance

init_existential
````````````````
::

  %1 = init_existential $T, %0
  ; %0 must be of a $*P type for protocol or protocol composition type P
  ; $T must be a type that fulfills protocol(s) P
  ; %1 will be of type $*T

Prepares the uninitialized existential container pointed to by ``%0`` to
contain a value of type ``$T``. ``%0`` must point to uninitialized storage
for an existential container. The result of the instruction is the address
of the concrete value inside the container; this storage is uninitialized and
must be initialized by a ``store`` or ``copy_addr`` to ``%1``. If the concrete
value must be deallocated without be initialized (for instance, if its
constructor fails), ``deinit_existential`` can do so. Once the concrete value
is initialized, the entire existential container can be destroyed with
``destroy_addr``.

deinit_existential
``````````````````
::

  deinit_existential %0
  ; %0 must be of a $*P type for protocol or protocol composition type P

Undoes the internal allocation (if any) performed by
``init_existential``.  This does not destroy the value referenced by
the existential container, which must be uninitialized.
``deinit_existential`` is only necessary for existential
containers that have been partially initialized by ``init_existential``
but haven't had their value initialized. A fully initialized existential can
be destroyed with ``destroy_addr`` like a normal address-only value.

protocol_method
```````````````
::

  %1 = protocol_method %0, @method
  ; %0 must be of a $*P type for protocol or protocol composition type P
  ; @method must be a reference to a method of (one of the) protocol(s) P
  ; %1 will be of type $Builtin.RawPointer -> T -> U
  ;   for method type T -> U

Obtains a reference to the function implementing protocol method ``@method``
for the concrete value referenced by the existential container
referenced by ``%0``. The resulting function value will take a pointer
to the ``this`` value as a ``RawPointer``. The ``this`` pointer value can
be derived from the existential container with a ``project_existential``
instruction.

project_existential
```````````````````
::

  %1 = project_existential %0
  ; %0 must be of a $*P type for protocol or protocol composition type P
  ; %1 will be of type $Builtin.RawPointer

Obtains a ``RawPointer`` pointing to the concrete value referenced by the
existential container referenced by ``%0``. This raw pointer can be passed to
protocol methods obtained by ``protocol_method``. A method call on a
protocol-type value in Swift::

  protocol Foo {
    func bar(x:Int)
  }

  var foo:Foo
  // ... initialize foo
  foo.bar(123)

compiles to this SIL::

  ; ... initialize %foo
  %bar = protocol_method %foo, @Foo.bar
  %foo_p = project_existential %foo
  %one_two_three = integer_literal $Builtin.Int64, 123
  %_ = apply %bar(%foo_p, %one_two_three)

Functions
~~~~~~~~~

closure
```````
::

  %C = closure %0(%1, %2, ...)
  ; %0 must be of a concrete function type $(A1, A2, ...) -> R
  ; %1, %2, etc. must be of the types of the first N arguments to %0
  ; %C will be of the function type of %0 with the first N arguments removed

Allocates a closure by partially applying the function ``%0`` in its first
N arguments. The closure will be a allocated as a box with retain count 1
containing the values ``%1``, ``%2``, etc. The closed-over values will not be
retained; that must be done separately if necessary.

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

Branching instructions terminate a basic block. Every basic block must end
with a branching instruction.

unreachable
```````````
::

  unreachable

Indicates that control flow must not reach the end of the current basic block.

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

unwind
``````
TBD

Calling convention
------------------

Calling a function with trivial value types as inputs and outputs simply passes
everything by value. This Swift function::

  func foo(x:Int, y:Float) -> Char

gets called in SIL as::

  %foo = constant_ref $(Int, Float) -> Char, @foo
  %z = apply %foo(%x, %y)

Reference type arguments get retained, and reference type return values must
be released. Value types with reference type components have their reference
type components retained and released the same way. This Swift function::

  class A {}

  func bar(x:A) -> (Int, A)

gets called in SIL as::

  %bar = constant_ref $(A) -> (Int, A), @bar
  retain %x
  %z = apply %bar(%x)
  ; ... use %z ...
  %z.1 = extract %z, 1
  release %z.1

Address-only arguments are passed by address and are callee-copied. The
caller maintains ownership of the referenced arguments, and the callee must
not modify them. An address-only return value is handled by passing the address
of a caller-owned uninitialized buffer as the final argument to the callee. The
callee must initialize this buffer before returning. This Swift function::

  struct [API] A {}

  func bas(x:A, y:Int) -> A

gets called in SIL as::

  %bas = constant_ref $(*A, Int, *A) -> ()
  %z = alloc_var stack $A
  apply %bas(%x, %y, %z)
  ; ... use %z ...
  destroy_addr %z
  dealloc_var stack %z

Examples
--------

Trivial example
~~~~~~~~~~~~~~~

A simple Swift function::

  struct FragileType { }
  func f(a:FragileType) -> FragileType

  func foo(b:Int) {
    var a = b
    f(a)
  }

will be emitted as the following SIL::

  ; decl "func foo"
  func @foo: $(FragileType) -> () {
  entry(%b:$FragileType):
    ; prologue
    %b_alloc = alloc_box $FragileType
    store %b to %b_alloc#1

    ; decl "var a"
    %a_alloc = alloc_box $FragileType
    ; expression "b"
    %1 = load %b_alloc#1
    ; initializer "var a = b"
    store %1 to %a_alloc#1

    ; expression "a"
    %2 = load %a

    ; expression "f"
    %3 = constant_ref $(FragileType) -> FragileType, @f

    ; expression "f(a)"
    %4 = apply %3(%2)

    ; cleanup for block
    release %a_alloc#0

    ; epilogue
    release %b_alloc#0
    %void = tuple ()
    return %void
  }

Note that all the memory management and allocation implicit to the Swift code
is made explicit in the SIL codegen. Optimization will simplify that into this::

  func @foo: $(FragileType) -> () {
  entry(%b:FragileType):
    %b_dbg = alloc_var pseudo $FragileType
    store %b to %b_dbg

    %a_dbg = alloc_var pseudo $FragileType
    store %b to %a_dbg

    %f = constant_ref $(FragileType) -> FragileType, @f
    %1 = apply %f(%b)

    %void = tuple ()
    return %void
  }

Escape analysis detects that the boxes allocated for ``a`` and ``b``
are unnecessary and eliminates them, replacing them with a ``pseudo``
stack allocation for debugging purposes.

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
    store %x to %x_alloc#1

    ; expression "func(y)..."
    %1 = constant_ref $(SIL.Box, *Int, Int) -> Int, \
                      @adder_1
    retain %x_alloc#0
    %2 = closure %1(%x_alloc#0, %x_alloc#1)

    ; epilogue
    release %x_box
    return %2
  }

  ; decl for anonymous function
  func @adder_1: $(SIL.Box, *Int, Int) -> Int {
  entry(%x_box:SIL.Box, %x_addr:*Int, %y:Int):
    ; prologue
    %y_alloc = alloc_box $Int
    store %y to %y_alloc#1

    ; expression "x"
    %1 = load %x_addr
    ; expression "y"
    %2 = load %y_alloc#1
    ; expression "+"
    %3 = constant_ref $(Int, Int) -> Int, @+
    ; expression "x + y"
    %4 = apply %3(%1, %2)

    ; epilogue
    release %y_alloc#0
    return %4
  }

The closed-over variable is represented as a pair of parameters to
the closure, the box holding the variable's reference count and the address
of the variable inside the box. The outer function retains the box explicitly
before embedding it in the closure with a ``closure`` instruction. In this case,
the variable ``x`` is not modified, so optimization can reduce the box capture
to a direct value capture::

  func @adder: $(Int) -> (Int) -> Int {
  entry(%x:Int):
    %x_dbg = alloc_var pseudo $Int
    store %x to %x_dbg
    %1 = constant_ref $(Int, Int) -> Int, @adder_1
    %2 = closure %1(%x)
    return %2
  }

  func @adder_1: $(Int, Int) -> Int {
  entry(%x:Int, %y:Int):
    %x_dbg = alloc_var pseudo $Int
    store %x to %x_dbg
    %y_dbg = alloc_var pseudo $Int
    store %y to %y_dbg
    %1 = constant_ref $(Int, Int) -> Int, @+
    %2 = apply %1(%x, %y)
    return %2
  }

TODO: more optimizations

* constant propagation into closure
* capture deletion
* recursive closure
* inlining

Resilient value types
~~~~~~~~~~~~~~~~~~~~~

A function that operates on a resilient type::

  struct [API] Point {
    var x:Float
    var y:Float

    constructor(x:Float, y:Float)
  }

  func reflect(point:Point) {
    var reflected = Point(-point.x, -point.y)
    return reflected
  }

will be emitted as SIL that operates on addresses of the type indirectly::

  func @reflect: $(*Point, *Point) {
  entry(%point:*Point, %ret:*Point):
    ; prologue
    %point_alloc = alloc_box $Point
    copy_addr %point to %point_alloc#1 ; copy_addr, not load/store

    ; decl "var reflected"
    %reflected_alloc = alloc_box $Point

    ; expression "point.x"
    %1 = constant_ref $(*Point) -> Float, @"Point.x get"
    %2 = apply %1(%point_alloc#1)
    ; expression "-point.x"
    %3 = constant_ref $(Float) -> Float, @-
    %4 = apply %3(%2)

    ; expression "point.y"
    %5 = constant_ref $(*Point) -> Float, @"Point.y get"
    %6 = apply %5(%point_alloc#1)
    ; expression "-point.y"
    %7 = constant_ref $(Float) -> Float, @-
    %8 = apply %7(%6)

    ; expression "Point"
    %9 = metatype $Point
    %10 = constant_ref $(Point.metatype) -> (Float, Float) \
                                         -> *Point, \
                       @constructor
    %11 = apply %10(%3)

    ; expression "Point(-point.x, -point.y)"
    %12 = apply %11(%4, %8)

    ; initializer "var reflected = ..."
    copy_addr %12 to %reflected_alloc#1
    ; cleanup temporary return
    destroy_addr %12
    dealloc_var heap %12

    ; statement "return reflected"
    copy_addr %reflected_alloc#1 to %ret

    ; cleanup for block
    release %reflected_alloc#0

    ; epilogue
    release %point_alloc#0
    return
  }

Note that although resilient types are manipulated through pointers, they still
have value semantics, so assigning and passing resilient values still incurs
allocations and copies as with loadable fragile types, although many value
semantics operations can be eliminated by optimization. For instance, since
the temporary value ``%12`` is destroyed immediately after being copied into a
variable, it can be combined into the ``copy_addr`` as a ``take`` operation::

    copy_addr take %12 to %reflected_alloc#1
    dealloc_var heap %12

TODO: more examples
~~~~~~~~~~~~~~~~~~~

* generics
* resilient-inside-fragile type

TODO design questions
---------------------

* debug information representation
* maintaining good AST location info in the face of optimization
