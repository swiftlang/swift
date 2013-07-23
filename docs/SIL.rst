.. @raise litre.TestsAreMissing

Swift Intermediate Language (SIL)
=================================

.. contents::

Abstract
--------

SIL is a SSA-form IR with high-level semantic information designed to implement
the Swift programming language. SIL accommodates the following use cases:

- High-level optimization passes, including retain/release optimization,
  dynamic method devirtualization, closure inlining, memory allocation
  promotion, and generic function instantiation;
- A set of guaranteed optimizations that provide a predictable baseline for
  runtime and diagnostic behavior;
- Diagnostic dataflow analysis passes that enforce Swift language requirements,
  such as definitive initialization of variables and constructors, code
  reachability, switch coverage; and
- A stable distribution format that can be used to distribute "fragile"
  inlineable or generic code with Swift library modules, to be optimized into
  client binaries.

SIL in the Swift Compiler
-------------------------

At a high level, the Swift compiler follows a strict pipeline architecture:

- The *Parse* module constructs an AST from Swift source code.
- The *Sema* module type-checks the AST and annotates it with type information.
- The *SILGen* module generates "raw" SIL from an AST.
- SIL *Passes* run over the raw SIL to emit diagnostics and apply optimizations
  to produce canonical SIL.
- *IRGen* lowers optimized SIL to LLVM IR.
- The LLVM backend applies LLVM optimizations and emits binary code.

The different stages pertaining especially to SIL processing are as follows:

SILGen
~~~~~~

SILGen produces "raw" SIL by walking a type-checked Swift AST. The form of SIL
emitted by SILGen has the following properties:

- Variables are represented by loading and storing mutable memory locations
  instead of being in strict SSA form. This is similar to the LLVM IR emitted
  by frontends such as Clang. However, Swift represents variables as
  reference-counted "boxes" in the most general case, which can be retained,
  released, and shared.
- Dataflow requirements, such as definitive assignment, function returns,
  switch coverage, etc. have not yet been enforced.
- ``always_inline``, ``always_instantiate``, and other function optimization
  attributes have not yet been honored.

These properties are addressed by subsequent guaranteed optimization and
diagnostic passes which are always run against the raw SIL.

Guaranteed Optimization Passes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After SILGen, a deterministic sequence of optimization passes is run over the
raw SIL, as follows:

- Memory promotion. This is a combination of an LLVM-style "mem2reg" pass to
  reduce in-memory variables to SSA values with a capture analysis mechanism
  for eliminating reference-counted boxes.

TODO:

- Always inline
- Constant folding/guaranteed simplifications (including constant overflow
  warnings)

Diagnostic Passes
~~~~~~~~~~~~~~~~~

The following passes are run after guaranteed optimization to diagnose the
validity of the Swift program that generated the SIL:

- Return analysis. This verifies that functions always return a value on every
  code path and don't "fall of the end" of their definition, which is an error.

TODO:

- Noreturn verification as a part of return analysis.
- Switch statement coverage.
- Dead code detection/elimination. Non-implicit dead code is an error.
- Definitive assignment of local variables, and of instance variables in
  constructors.
- Basic ARC optimization for decent performance at -O0.

If the diagnostic passes all succeed, the final result is the *canonical SIL*
for the program. Performance optimization, native code generation, and module
distribution are derived from this form.

Syntax
------

SIL is reliant on Swift's type system and declarations, so SIL syntax is
an extension of Swift's. A ``.sil`` file is a Swift source file with added
SIL definitions. The Swift source is parsed only for its declarations;
Swift ``func`` bodies and top-level code are ignored except for nested
declarations. In a ``.sil`` file, there are no implicit imports; the ``swift``
and/or ``Builtin`` standard modules must be imported explicitly if used.

Here is an example of a ``.sil`` file::

  import swift

  // Define a type used by the SIL function.
  struct Point {
    var x : Double
    var y : Double
  }

  // Declare a Swift function. The body is ignored by SIL.
  func taxicabNorm(a:Point) -> Double {
    return a.x + a.y
  }

  // Define a SIL function.
  // The name @_T5norms11taxicabNormfT1aV5norms5Point_Sd is the mangled name
  // of the taxicabNorm Swift function.
  sil @_T5norms11taxicabNormfT1aV5norms5Point_Sd : $(Point) -> Double {
  bb0(%0 : $Point):
    // func swift.+(Double, Double) -> Double
    %1 = function_ref @_TSsoi1pfTSdSd_Sd
    %2 = struct_extract %0 : $Point, #Point.x
    %3 = struct_extract %0 : $Point, #Point.y
    %4 = apply %1(%2 : $Double, %3 : $Double) : $(Double, Double) -> Double
    %5 = return %4 : Double
  }

SIL Types
~~~~~~~~~
::

  sil-type ::= '$' '*'? generic-parameter-list? type

SIL types are introduced with the ``$`` sigil. SIL's type system is a superset
of Swift's, and so the type after the ``$`` is parsed using Swift's
type grammar. SIL adds some additional kinds of type of its own:

- The *address of T* ``$*T``, a pointer to memory containing a
  value of any reference or value type ``$T``.  This can be an internal pointer
  into a data structure. Addresses of loadable types can be loaded and stored
  to access values of those types.
  Addresses of address-only types (see below) can only be used with
  instructions that manipulate their operands indirectly by address, such
  as ``copy_addr``, ``destroy_addr``, and ``dealloc_var``, or as arguments
  to functions. Addresses cannot be retained or released.
- Values of *generic function type* such as
  ``$<T...> (A...) -> R`` can be expressed in SIL.  Accessing a generic
  function with ``function_ref`` will give a value of a generic function type.
  Its type variables can be bound with a ``specialize`` instruction to
  give a value of a *concrete function type* ``$(A...) -> R``.

SIL classifies types into additional subgroups based on ABI stability:

- *Loadable types* are types with a fully exposed concrete representation:

  * Reference types
  * Builtin value types
  * Fragile struct types in which all element types are loadable
  * Tuple types in which all element types are loadable
  * Class protocol types
  * Archetypes constrained by a class protocol

  A *loadable aggregate type* is a tuple or struct type that is loadable.

- *Address-only types* are restricted value types for which the compiler
  cannot access a full concrete representation:

  * Resilient value types
  * Fragile struct or tuple types that contain resilient types as elements at
    any depth
  * Archetypes not constrained by a class protocol
  * Non-class protocol types

  Values of address-only types must reside in memory and can only be referenced
  in SIL by address. Address-only type addresses cannot be loaded from or
  stored to. SIL provides special instructions for indirectly accessing
  address-only values.

Swift types may not translate one-to-one to SIL types. In particular, tuple
types are canonicalized, and function types are canonicalized and mangled in
order to encode calling convention and resilience rules. Function input argument
tuples are flattened.

Values and Operands
~~~~~~~~~~~~~~~~~~~
::

  sil-identifier ::= [A-Za-z_0-9]+
  sil-value ::= '%' sil-identifier
  sil-operand ::= sil-value ('#' [0-9]+)? ':' sil-type

SIL values are introduced with the ``%`` sigil and named by an
alphanumeric identifier, which references the instruction or basic block
argument that produces the value. When used as an operand, the reference
is always followed by a ``:`` and the SIL type of the value. For example::

  // Produce a function and integer value with builtin and integer_literal
  %negate = builtin_function_ref #Builtin.neg_Int64
  %five = integer_literal 5 : $Builtin.Int64
  // Use the values as operands
  %neg_five = apply %negate(%five : $Builtin.Int64) : (Builtin.Int64) -> Builtin.Int64

In SIL, a single instruction may produce multiple values. Operands that refer
to multiple-value instructions choose the value by following the ``%name`` with
``#`` and the index of the value. For example::

  // alloc_box produces two values--the refcounted pointer %box#0, and the
  // value address %box#1
  %box = alloc_box $Int64
  // Refer to the refcounted pointer
  %1 = retain %box#0
  // Refer to the address
  store %box#1, %value

Unlike LLVM IR, SIL instructions that take value operands *only* accept
value operands. References to literal constants, functions, global variables, or
other entities require specialized instructions such as ``integer_literal``,
``function_ref``, ``global_addr``, etc.

Functions
~~~~~~~~~
::

  sil-function ::= 'sil' sil-function-name ':' sil-type '{' sil-basic-block+ '}'
  sil-function-name ::= '@' [A-Za-z_0-9]+

SIL functions are introduced at the top level with the ``sil`` keyword. SIL
function names are introduced with the ``@`` sigil and named by an
alphanumeric identifier. This name is usually the mangled name of a Swift
function. The ``sil`` syntax declares the function's name and SIL type then
defines the body of the function inside braces. The declared type must be a
function type, which may be generic.

Basic Blocks
~~~~~~~~~~~~
::

  sil-basic-block ::= sil-label sil-instruction-def* sil-terminator-def
  sil-label ::= sil-identifier ('(' sil-argument (',' sil-argument)* ')')? ':'
  sil-instruction-def ::= sil-value '=' sil-instruction
  sil-terminator-def ::= sil-value '=' sil-terminator

A function body consists of one or more basic blocks. These form the nodes of
the control flow graph. Each basic block contains one or more instructions and
is terminated by a terminator instructor—either a branch to another block,
a return, or an ``unreachable`` marker. The entry point for the function is
always the first basic block in its body.

Basic blocks can take arguments. The entry point block's argument values are
received from the function caller::

  sil @foo : $(Int) -> Int {
  bb0(%x : $Int):
    %1 = return %x : $Int
  }

  sil @bar : $(Int, Int) -> () {
  bb0(%x : $Int, %y : $Int):
    %foo = function_ref @foo
    %1 = apply %foo(%x : $Int) : $(Int) -> Int
    %2 = apply %foo(%y : $Int) : $(Int) -> Int
    %3 = tuple ()
    %4 = return %3 : $()
  }

Arguments for other basic blocks are bound by the branch instructions that
transfer control to that block. This is how SIL expresses branching dataflow in
SSA as an alternative to phi instructions::

  sil @iif : $(Builtin.Int1, Builtin.Int64, Builtin.Int64) -> Builtin.Int64 {
  bb0(%cond : $Builtin.Int1, %then : $Builtin.Int64, %else : $Builtin.Int64):
    condbranch %cond : $Builtin.Int1, then, else
  then:
    br finish(%then : $Builtin.Int64)
  else:
    br finish(%else : $Builtin.Int64)
  finish(%result : $Builtin.Int64):
    ret %result : $Builtin.Int64
  }

Declaration References
~~~~~~~~~~~~~~~~~~~~~~
::

  sil-decl-ref ::= '#' sil-identifier ('.' sil-identifier)* sil-decl-subref?
  sil-decl-subref ::= '!' sil-decl-subref-part ('.' sil-decl-uncurry-level)? ('.' sil-decl-lang)?
  sil-decl-subref ::= '!' sil-decl-uncurry-level ('.' sil-decl-lang)?
  sil-decl-subref ::= '!' sil-decl-lang
  sil-decl-subref-part ::= 'getter'
  sil-decl-subref-part ::= 'setter'
  sil-decl-subref-part ::= 'allocator'
  sil-decl-subref-part ::= 'initializer'
  sil-decl-subref-part ::= 'oneofelt'
  sil-decl-subref-part ::= 'destroyer'
  sil-decl-subref-part ::= 'globalaccessor'
  sil-decl-subref-part ::= 'defaultarg' '.' [0-9]+
  sil-decl-uncurry-level ::= [0-9]+
  sil-decl-lang ::= 'objc'

Some SIL instructions need to reference Swift declarations directly. These
references are introduced with the ``#`` sigil followed by the fully qualified
dotted path naming the Swift declaration. Some Swift declarations are
decomposed into multiple entities at the SIL level. These are discriminated by
following the qualified name with a ``!`` then naming the component entity:

- ``getter`` references the getter function for a ``var`` declaration.
- ``setter`` references the setter function for a ``var`` declaration.
- ``allocator`` references the allocating constructor for a class's
  ``constructor`` declaration, or the constructor for a struct or oneof's
  ``constructor``.
- ``initializer`` references the allocating constructor for a class's
  ``constructor`` declaration.
- ``oneofelt`` references a member of a oneof type.
- ``destroyer`` references the destroying destructor for a class's
  ``destructor`` declaration.
- ``globalaccessor`` references the addressor function for a global variable.
- ``defaultarg.<n>`` references the default argument generating function for
  the ``<n>``-th argument of a Swift ``func``.

Methods and curried function definitions in Swift also have multiple "uncurry
levels" in SIL, representing the function at each possible partial application
level.

Functions may also have multiple entry points for foreign language interop which
can be discriminated. Currently ``objc`` is the only such discriminator.

Instruction Set
---------------

In the instruction descriptions, ``[optional attributes]`` appear in square
brackets, and ``{required|attribute|choices}`` appear in curly braces with
options separated by pipes. Variadic operands are indicated with ``...``.

Allocation and Deallocation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

These instructions allocate and deallocate memory.

alloc_stack
```````````
::

  sil-instruction ::= 'alloc_stack' sil-type

  %1 = alloc_stack $T
  // %1 has type $*T

Allocates enough uninitialized memory on the stack to contain a value of type
``T``. The result of the instruction is the address
of the allocated memory. ``alloc_stack`` marks the start of the lifetime of
the value; the allocation must be balanced with a ``dealloc_stack``
instruction to mark the end of its lifetime. The memory is not retainable;
to allocate a retainable box for a value type, use ``alloc_box``.

alloc_ref
`````````
::

  sil-instruction ::= 'alloc_ref' sil-type

  %1 = alloc_ref $T
  // $T must be a reference type
  // %1 has type $T

Allocates an object of reference type ``T``. The object will be initialized
with retain count 1; its state will be otherwise uninitialized.

alloc_box
`````````
::
  
  sil-instruction ::= 'alloc_box' sil-type

  %1 = alloc_box $T
  // %1 has two values:
  //   %1#0 has type Builtin.ObjectPointer
  //   %1#1 has type *T

Allocates a reference-counted "box" on the heap large enough to hold a value of
type ``T``. The result of the instruction is a two-value operand;
the first value is the reference-counted ``ObjectPointer`` that owns the box,
and the second value is the address of the value inside the box.
The box will be initialized with a retain count of 1; the storage will be
uninitialized. The box owns the contained value, and releasing it to a retain
count of zero destroys the contained value as if by ``destroy_addr``. Releasing
a box is thus invalid if the box's value is uninitialized. To deallocate a box
whose value has not been initialized, ``dealloc_ref`` should be used.

alloc_array
```````````
::

  sil-instruction ::= 'alloc_array' sil-type ',' sil-operand

  %1 = alloc_array $T, %0 : Builtin.Int<n>
  // $T must be a type
  // %0 must be of a builtin integer type
  // %1 has two values:
  //   %1#0 has type Builtin.ObjectPointer
  //   %1#1 has type *T

Allocates a box large enough to hold an array of ``%0`` values of type ``T``.
The result of the instruction is a two-value operand; the first value is the
reference-counted ``ObjectPointer`` that owns the box,
and the second value is the address of the first value inside the box.
The box will be initialized with a retain count of 1; the storage will be
uninitialized. The box owns the contained array of values, and releasing it
to a retain count of zero destroys all of the contained values as if by
``destroy_addr``. Releasing the array is thus invalid unless all of the array's
value have been uninitialized. To deallocate a box
whose value has not been initialized, ``dealloc_ref`` should be used.

dealloc_stack
`````````````
::

  sil-instruction ::= 'dealloc_stack' sil-operand

  dealloc_stack %0 : $*T
  // %0 must be of an address $*T type

Deallocates memory previously allocated by ``alloc_stack``. The value in memory
must be uninitialized or destroyed prior to being deallocated. This instruction
marks the end of the lifetime for the value created by the corresponding
``alloc_stack`` instruction.

dealloc_ref
```````````
::

  sil-instruction ::= 'dealloc_ref' sil-operand

  dealloc_ref %0 : $T
  // %0 must be of a box or reference type

Deallocates a box or reference type instance, bypassing the reference counting
mechanism. The box must have a retain count of one. This does not
destroy the reference type instance or the values inside the box. The contents
of the reference-counted instance must be fully initialized or destroyed before
``dealloc_ref`` is applied.

Literals
~~~~~~~~

These instructions bind SIL values to literal constants or to global entities.

function_ref
````````````
::

  sil-instruction ::= 'function_ref' sil-function-name ':' sil-type

  %1 = function_ref @function : $[thin] T -> U
  // $[thin] T -> U must be a thin function type
  // %1 has type $T -> U

Creates a reference to a SIL function.

builtin_function_ref
````````````````````
::

  sil-instruction ::= 'builtin_function_ref' sil-decl-ref ':' sil-type

  %1 = builtin_function_ref #Builtin.foo : $[thin] T -> U
  // #Builtin.foo must name a function in the Builtin module
  // $[thin] T -> U must be a thin function type
  // %1 has type $[thin] T -> U

Creates a reference to a compiler builtin function.

global_addr
```````````
::

  sil-instruction ::= 'global_addr' sil-decl-ref ':' sil-type

  %1 = global_addr #foo.bar : $*T
  // #foo.bar must name a physical global variable declaration
  // $*T must be an address type
  // %1 has type $*T

TODO: Design of global variables subject to change.

Creates a reference to the address of a global variable.

integer_literal
```````````````
::

  sil-instruction ::= 'integer_literal' sil-type ',' int-literal

  %1 = integer_literal $Builtin.Int<n>, 123
  // $Builtin.Int<n> must be a builtin integer type
  // %1 has type $Builtin.Int<n>

Creates an integer literal value. The result will be of type
``Builtin.Int<n>``, which must be a builtin integer type.

float_literal
`````````````
::

  sil-instruction ::= 'float_literal' sil-type ',' float-literal

  %1 = float_literal $Builtin.FP<n>, 1.23
  // $Builtin.FP<n> must be a builtin floating-point type
  // %1 has type $Builtin.FP<n>

Creates a floating-point literal value. The result will be of type ``
``Builtin.FP<n>``, which must be a builtin floating-point type.

string_literal
``````````````
::

  sil-instruction ::= 'string_literal' sil-type ',' string-literal

  %1 = string_literal $T, "asdf"
  // $T must be either $Builtin.RawPointer,
  //   or $(Builtin.RawPointer, Builtin.Int64)
  // %1 has type $T

Creates a reference to a string in the global string table. The value can be
either a lone ``Builtin.RawPointer`` referencing the start of the string, or
a ``(Builtin.RawPointer, Builtin.Int64)`` pair of both the start of
the string and its length. In either case, the referenced string is
null-terminated.

builtin_zero
````````````
::

  sil-instruction ::= 'builtin_zero' sil-type

  %1 = builtin_zero $T
  // $T must be either a reference type, or a Builtin type.
  // %1 has type $T

Creates the "zero" value of a builtin or reference type:

- For builtin integer types, this is equivalent to 0.
- For builtin floating-point types, this is equivalent to +0.0.
- For ``Builtin.RawPointer`` and ``Builtin.ObjectPointer``, this produces a
  null pointer.
- For reference types, this produces a null reference.

TODO: Design type-safe nullability for reference types.

Dynamic Dispatch
~~~~~~~~~~~~~~~~

These instructions perform dynamic lookup of class and generic methods. They
share a common set of attributes::

  sil-method-attributes ::= '[' 'volatile'? ']'

The ``volatile`` attribute on a dynamic dispatch instruction indicates that
the method lookup is semantically required (as, for example, in Objective-C).
When the type of a dynamic dispatch instruction's operand is known,
optimization passes can promote non-``volatile`` dispatch instructions
into static ``function_ref`` instructions.

If a dynamic dispatch instruction references an Objective-C method
(indicated by the ``objc`` language marker on a method reference, as in
``#NSObject.description!1.objc``), then the instruction
represents an ``objc_msgSend`` invocation. ``objc_msgSend`` invocations can
only be used as the callee of an ``apply`` instruction. They cannot be stored,
used as ``apply`` or ``partial_apply`` arguments, or as the callee of a
``partial_apply``. ``objc_msgSend`` invocations additionally must always be
``volatile``.

class_method
````````````
::

  sil-instruction ::= 'class_method' sil-method-attributes?
                        sil-operand ',' sil-decl-ref ':' sil-type

  %1 = class_method %0 : $T, #T.method!1 : $[thin] U -> V
  // %0 must be of a class type or class metatype $T
  // #T.method!1 must be a reference to a dynamically-dispatched method of T or
  // of one of its superclasses, at uncurry level >= 1
  // %1 will be of type $U -> V

Looks up a method based on the dynamic type of a class or class metatype
instance.

super_method
````````````
::

  sil-instruction ::= 'super_method' sil-method-attributes?
                        sil-operand ',' sil-decl-ref ':' sil-type
  
  %1 = super_method %0 : $T, #Super.method!1.objc : $[thin] U -> V
  // %0 must be of a non-root class type or class metatype $T
  // #Super.method!1.objc must be a reference to an ObjC method of T's
  // superclass or ; of one of its ancestor classes, at uncurry level >= 1
  // %1 will be of type $[thin] U -> V

Looks up a method in the superclass of a class or class metatype instance.
Note that for native Swift methods, ``super.method`` calls are statically
dispatched, so this instruction is only valid for Objective-C methods.

archetype_method
````````````````
::

  sil-instruction ::= 'archetype_method' sil-method-attributes?
                        sil-type ',' sil-decl-ref ':' sil-type

  %1 = archetype_method $T, #Proto.method!1 : $[thin] U -> V
  // $T must be an archetype
  // #Proto.method!1 must be a reference to a method of one of the protocol
  // constraints on T
  // $U -> V must be the type of the referenced method with "This == T"
  // substitution applied
  // %1 will be of type $[thin] U -> V

Looks up the implementation of a protocol method for a generic type variable
constrained by that protocol.

protocol_method
```````````````
::

  sil-instruction ::= 'protocol_method' sil-method-attributes?
                        sil-operand ',' sil-decl-ref ':' sil-type

  %1 = protocol_method %0 : $P, #P.method!1 : $[thin] U -> V
  // %0 must be of a protocol or protocol composition type $P,
  //   address of address-only protocol type $*P,
  //   or metatype of protocol type $P.metatype
  // #P.method!1 must be a reference to a method of one of the protocols of P
  //
  // If %0 is an address-only protocol address, then the "this" argument of
  //   the method type $[thin] U -> V must be Builtin.OpaquePointer
  // If %0 is a class protocol value, then the "this" argument of
  //   the method type $[thin] U -> V must be Builtin.ObjCPointer
  // If %0 is a protocol metatype, then the "this" argument of
  //   the method type $[thin] U -> V must be P.metatype

Looks up the implementation of a protocol method for the dynamic type of the
value inside an existential container. The "this" operand of the result
function value is represented using an opaque type, the value for which must
be projected out of the same existential container as the ``protocol_method``
operand::

- If the operand is the address of an address-only protocol type, then the
  "this" argument of the method is of type ``Builtin.OpaquePointer``, and
  can be projected using the ``project_existential`` instruction.
- If the operand is a value of a class protocol type, then the "this"
  argument of the method is of type ``Builtin.ObjCPointer``, and can be
  projected using the ``project_existential_ref`` instruction.
- If the operand is a protocol metatype, it does not need to be projected, and
  the "this" argument of the method is the protocol metatype itself.

Function Application
~~~~~~~~~~~~~~~~~~~~

These instructions call functions or wrap them in partial application or
specialization thunks.

apply
`````
::

  sil-instruction ::= 'apply' sil-value
                        '(' (sil-operand (',' sil-operand)?)? ')'
                        ':' sil-type

  %r = apply %0(%1 : $A, %2 : $B, ...) : $(A, B, ...) -> R
  // Note that the type of the callee '%0' is specified *after* the arguments
  // %0 must be of a concrete function type $(A, B, ...) -> R
  // %1, %2, etc. must be of the argument types $A, $B, etc.
  // %r will be of the return type $R

Transfers control to function ``%0``, passing it the given arguments. The
input argument tuple type is destructured. The
``apply`` instruction does no retaining or releasing of its arguments by
itself; the calling convention's retain/release policy must be handled by
separate explicit ``retain`` and ``release`` instructions. The return value
will likewise not be implicitly retained or released. ``%0`` must be an object
of a concrete function type; generic functions must have all of their generic
parameters bound with a ``specialize`` instruction before they can be applied.

TODO: should have normal/unwind branch targets, like LLVM ``invoke``.

partial_apply
`````````````
::

  sil-instruction ::= 'partial_apply' sil-value
                        '(' (sil-operand (',' sil-operand)?)? ')'
                        ':' sil-type

  %c = partial_apply %0(%1 : $A, %2 : $B, ...) : $[thin] (T..., A, B, ...) -> R
  // Note that the type of the callee '%0' is specified *after* the arguments
  // %0 must be of a thin concrete function type $[thin] (T..., A, B, ...) -> R
  // %1, %2, etc. must be of the argument types $A, $B, etc.,
  //   of the tail part of the argument tuple of %0
  // %c will be of the partially-applied thick function type (T...) -> R

Creates a closure by partially applying the function ``%0`` to a partial
sequence of its arguments. The closure context will be allocated with retain
count 1 and initialized to contain the values ``%1``, ``%2``, etc.
The closed-over values will not be retained; that must be done separately before
the ``partial_apply``. The closure does take ownership of the partially applied
arguments.

This instruction is used to implement both curry thunks and closures. A
curried function in Swift::

  func foo(a:A)(b:B)(c:C)(d:D) -> E { /* body of foo */ }

emits curry thunks in SIL as follows (retains and releases omitted for
clarity)::

  func @foo : $[thin] A -> B -> C -> D -> E {
  entry(%a : $A):
    %foo_1 = function_ref @foo_1 : $[thin] (B, A) -> C -> D -> E
    %thunk = partial_apply %foo_1(%a : $A) : $[thin] (B, A) -> C -> D -> E
    return %thunk : $B -> C -> D -> E
  }

  func @foo_1 : $[thin] (B, A) -> C -> D -> E {
  entry(%b : $B, %a : $A):
    %foo_2 = function_ref @foo_2 : $[thin] (C, B, A) -> D -> E
    %thunk = partial_apply %foo_2(%b : $B, %a : $A) : $[thin] (C, B, A) -> D -> E
    return %thunk : $(B, A) -> C -> D -> E
  }

  func @foo_2 : $[thin] (C, B, A) -> D -> E {
  entry(%c : $C, %b : $B, %a : $A):
    %foo_3 = function_ref @foo_3 : $[thin] (D, C, B, A) -> E
    %thunk = partial_apply %foo_3(%c : $C, %b : $B, %a : $A) : $[thin] (D, C, B, A) -> E
    return %thunk : $(C, B, A) -> D -> E
  }

  func @foo_3 : $[thin] (D, C, B, A) -> E {
  entry(%d : $D, %c : $C, %b : $B, %a : $A):
    // ... body of foo ...
  }

A local function in Swift that captures context, such as ``bar`` in the
following example::

  func foo(x:Int) -> Int {
    func bar(y:Int) -> Int {
      return x + y
    }
    return bar(1)
  }

lowers to an uncurried entry point and is curried in the enclosing function::
  
  func @bar : $[thin] (Int, Int) -> Int {
  entry(%y : $Int, %x : $Int):
    // ... body of bar ...
  }

  func @foo : $[thin] Int -> Int {
  entry(%x : $Int):
    // Create the bar closure
    %bar_uncurried = function_ref @bar : $(Int, Int) -> Int
    %bar = partial_apply %bar_uncurried(%x : $Int) : $(Int, Int) -> Int

    // Apply it
    %1 = integer_literal $Int, 1
    %ret = apply %bar(%1 : $Int) : $(Int) -> Int

    // Clean up
    release %bar : $(Int) -> Int
    return %ret : $Int
  }

specialize
``````````
::
  
  sil-instruction ::= 'specialize' sil-operand ',' sil-type
                        (',' sil-substitution)+
  sil-substitution ::= type '=' type
  
  %1 = specialize %0 : $[thin] <A, B, C> T -> U, $T1 -> U1, A = A1, B = B1, ...
  // %0 must be of a thin generic function type $[thin] <A, B, C> T -> U
  // $T1 -> U1 must be the thick concrete function type $T1 -> U1, where
  //   T1 == T and U1 == U after substitutions A == A1, B == B1, etc.

Specializes a generic function ``%0`` to a concrete function type
by binding its generic type variables with the given substitutions. The
conversion thunk includes loading non-address-only concrete arguments from
address-only arguments (in other words, an address-only argument of type $*T
will be mapped to a loadable value argument of type $U).

Metatypes
~~~~~~~~~

These instructions access metatypes, either statically by type name or
dynamically by introspecting class or generic values.

metatype
````````
::

  sil-instruction ::= 'metatype' sil-type

  %1 = metatype $T.metatype
  // %1 has type $T.metatype

Creates a reference to the metatype object for type ``T``.

class_metatype
``````````````
::

  sil-instruction ::= 'class_metatype' sil-type ',' sil-operand

  %1 = class_metatype $T.metatype, %0 : $T
  // %0 must be of a class type $T
  // %1 will be of type $T.metatype and reference the runtime metatype of %0

Obtains a reference to the dynamic metatype of the class instance ``%0``.

archetype_metatype
``````````````````
::

  sil-instruction ::= 'archetype_metatype' sil-type ',' sil-operand

  %1 = archetype_metatype $T.metatype, %0 : $T
  // %0 must be a value of class archetype $T, or the address of
  // an address-only archetype $*T
  // %1 will be of type $T.metatype

Obtains a reference to the dynamic metatype of the archetype value ``%0``.

protocol_metatype
`````````````````
::

  sil-instruction ::= 'protocol_metatype' sil-type ',' sil-operand

  %1 = protocol_metatype $P.metatype, %0 : $P
  // %0 must be a value of class protocol or protocol composition
  //   type $P, or an address of address-only protocol type $*P
  // %1 will be a $P.metatype value referencing the metatype of the
  //   concrete value inside %0

Obtains the metatype of the concrete value
referenced by the existential container referenced by ``%0``.

associated_metatype
```````````````````
::

  sil-instruction ::= 'associated_metatype' sil-operand ',' sil-type

  %1 = associated_metatype %0 : $T.metatype, $T.U.metatype
  // %0 must be a metatype value of type $T.metatype
  // $T.U must be an associated type of $T
  // %1 has type $T.U.metatype

Obtains the metatype object for the associated type ``$T.U`` of the type with
metatype ``%0``.

TODO: This doesn't need to be different from ``metatype``.

TODO To Be Updated
~~~~~~~~~~~~~~~~~~

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

initialize_var
``````````````
::

  initialize_var %0
  ; %0 must be an address $*T

TODO: Dataflow analysis not implemented yet. initialize_var currently just
does a zero initialization.

A pseudo-instruction that semantically "stores" a pseudo-value to the address
``%0`` representing the default state of a variable without an initializer.
Dataflow analysis must replace this instruction in one of the following ways:

- If there is a definitive assignment to ``%0`` along every code path
  dominated by the ``initialize_var``, those assignments become
  initializations of ``%0``. A definitive assignment is a store to ``%0`` that
  precedes any use of the pseudo-value loaded from ``%0`` other than as the
  operand of ``retain`` or ``release`` or as the destination
  for ``copy_addr assign``. For example, this definitive assignment sequence
  for a reference type::
    
    ; Foo is a class type
    %x = alloc_var stack $Foo
    initialize_var %x
    ; Reassignment sequence
    %x.old = load %x
    retain %y
    store %y to %x
    release %x.old

  becomes an initialization sequence::

    %x = alloc_var stack $Foo
    retain %y
    store %y to %x

  Likewise, in this definitive assignment sequence for an address-only type::

    ; T is an archetype
    %x = alloc_var stack $T
    initialize_var %x
    copy_addr %y to assign %x

  the ``copy_addr`` becomes an initialization::

    %x = alloc_var stack $T
    copy_addr %y to %x

- If dataflow analysis fails to find a definitive assignment for ``%0`` and the
  type referenced by ``%0`` has a default constructor, then ``initialize_var``
  becomes a call to the default constructor for the type referenced by ``%0``,
  with its result stored to ``%0``. So in this sequence, in which the
  ``initialize_var`` pseudo-value is used before being stored over::

    ; Foo is a struct type with default constructor
    %x = alloc_var stack $Foo
    initialize_var %x
    ; Pass the initialized x to a function
    %x.value = load %x
    %bar = constant_ref $(Foo) -> (), @bar
    apply %bar(%x.value)
    ; Store a new value to x
    %bas = constant_ref $() -> Foo, @bas
    %y = apply %bas()
    store %y to %x

  the ``initialize_var`` becomes a constructor call::

    %x = alloc_var stack $Foo
    %constructor = constant_ref $(Foo.metatype) -> () -> Foo, @Foo.constructor
    %Foo = metatype $Foo
    %constructor.0 = apply %constructor(%Foo)
    %x.init = apply %constructor.0()
    store %x.init to %x
    ; ...

  and the subsequent code continues normally.

If neither definitive assignment nor default construction are possible, then
dataflow analysis of ``initialize_var`` raises an error. ``initialize_var``
cannot be lowered to IR.

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

struct
``````

tuple
`````

module
``````
::

  %1 = module @M
  ; @M must be a module name
  ; %1 has type $module<M>

Creates a module value referencing module ``M``.

tuple_extract
`````````````
::

  %1 = tuple_extract %0, 123
  ; %0 must be of a loadable aggregate type
  ; %1 will be of the type of the 123rd element of %0

Extracts an element of a loadable aggregate value.

tuple_element_addr
``````````````````
::

  %1 = tuple_element_addr %0, 123
  ; %0 must of a $*T type for a loadable aggregate type T
  ; %1 will be of type $*U where U is the type of the 123rd
  ;   element of T

Given the address of a loadable aggregate value in memory, creates a
value representing the address of an element within that value.

struct_extract
``````````````

struct_element_addr
```````````````````

ref_element_addr
````````````````
::

  %1 = ref_element_addr %0, @T.x
  ; %0 must be of a reference type $T
  ; @T.x must be an instance field of $T
  ; %1 will be of type $*U where U is the type of the 123rd
  ;   element of T

Given a value of a reference type, creates a value representing the address
of an element within the referenced instance.

project_existential
```````````````````
::

  %1 = project_existential %0
  ; %0 must be of a $*P type for protocol or protocol composition type P
  ; %1 will be of type $Builtin.OpaquePointer

Obtains an ``OpaquePointer`` pointing to the concrete value referenced by the
existential container referenced by ``%0``. This pointer can be passed to
protocol instance methods obtained by ``protocol_method`` from the same
existential container. A method call on a protocol-type value in Swift::

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

It is an error if the result of ``project_existential`` is used as anything
other than the "this" argument of an instance method reference obtained by
``protocol_method`` from the same existential container.

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

upcast_existential
``````````````````

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

project_existential_ref
```````````````````````

init_existential_ref
````````````````````

upcast_existential_ref
``````````````````````
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

convert_function
````````````````
::

  %1 = convert_function %0, $T
  ; %0 must be of a function type $U ABI-compatible with $T
  ; %1 will be of type $T

Performs a conversion of the function ``%0`` to type ``T``, which must be ABI-
compatible with the type of ``%0``. Function types are ABI-compatible if their
input and/or result types are tuple types that differ only in label names or
default values.

coerce
``````
::

  %1 = coerce %0, $T
  ; %0 must be of type $T
  ; %1 will be of type $T

Represents an explicit type coercion with no runtime effect. ``%1`` will be
equivalent to ``%0``.

upcast
``````

address_to_pointer
``````````````````
::

  %1 = address_to_pointer %0
  ; %0 must be of an address type $*T
  ; %1 will be of type Builtin.RawPointer

Creates a ``Builtin.RawPointer`` value corresponding to the address ``%0``.

pointer_to_address
``````````````````

ref_to_object_pointer
`````````````````````

object_pointer_to_ref
`````````````````````

ref_to_raw_pointer
``````````````````

raw_pointer_to_ref
``````````````````

thin_to_thick_function
``````````````````````

convert_cc
``````````

bridge_to_block
```````````````

archetype_ref_to_super
``````````````````
::

  %1 = archetype_to_super %0, $T
  ; %0 must be an address of an archetype $*U with base class constraint U : B
  ; $T must be the base constraint type B or a superclass of B
  ; %1 will be of the base type $T

Performs an upcast operation on the archetype value referenced by ``%0``.

super_to_archetype_ref
``````````````````````
::

  super_to_archetype %0 to %1
  ; %0 must be of a reference type $T
  ; %1 must be the address of an archetype $*U with base class constraint U : B
  ;   where B is T or a subclass of T

Performs a checked downcast operation on the class instance referenced by
``%0``, initializing the archetype referenced by ``%1`` with a reference to
the class instance if the check succeeds.

FIXME: if it fails...

downcast
````````
::

  %1 = downcast %0, $T
  ; %0 must be of a reference type that is a subclass of $T
  ; $T must be a class type
  ; %1 will be of type T

Performs a checked downcast conversion of ``%0`` to subclass ``T``.

FIXME: if it fails...

downcast_archetype_addr
```````````````````````

downcast_archetype_ref
``````````````````````

project_downcast_existential_addr
`````````````````````````````````

downcast_existential_ref
````````````````````````

is_nonnull
``````````

index_addr
``````````
::

  %2 = index_addr %0, %1
  ; %0 must be of a $*T type
  ; %1 must be of a builtin integer type
  ; %2 will be of the same $*T type as %0

Given a pointer into an array of values, returns the address of the
``%1``-th element relative to ``%0``.

index_raw_pointer
`````````````````

Terminators
~~~~~~~~~~~

These instructions terminate a basic block. Every basic block must end
with a terminator.

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

autorelease_return
``````````````````

br
``
::

  br label (%0, %1, ...)
  ; `label` must refer to a block label within the current function
  ; %0, %1, etc. must be of the types of `label`'s arguments

Unconditionally transfers control from the current basic block to the block
labeled ``label``, passing the given values as arguments to ``label``.

condbranch
``````````
::

  condbranch %0, true_label (%T1, %T2, ...),
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

switch_oneof
````````````

Protocol and protocol composition types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FIXME: Re-section instructions.

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


Calling convention
------------------

Calling a function with trivial value types as inputs and outputs simply passes
the arguments by value. This Swift function::

  func foo(x:Int, y:Float) -> Char

  foo(x, y)

gets called in SIL as::

  %foo = constant_ref $(Int, Float) -> Char, @foo
  %z = apply %foo(%x, %y)

Reference type arguments get retained, and reference type return values must
be released. Value types with reference type components have their reference
type components retained and released the same way. This Swift function::

  class A {}

  func bar(x:A) -> (Int, A)

  bar(x)

gets called in SIL as::

  %bar = constant_ref $(A) -> (Int, A), @bar
  retain %x
  %z = apply %bar(%x)
  ; ... use %z ...
  %z.1 = extract %z, 1
  release %z.1

For address-only arguments, the caller allocates a copy and passes the address
of the copy to the callee. The callee takes ownership of the copy and is
responsible for destroying or consuming the value, though the caller must
deallocate the memory. For address-only return values, the
caller allocates an uninitialized buffer and passes its address as the final
argument to the callee. The callee must initialize this buffer before
returning. This Swift function::

  struct [API] A {}

  func bas(x:A, y:Int) -> A { return x }

  var z = bas(x, y)
  // ... use z ...

gets called in SIL as::

  %bas = constant_ref $(*A, Int, *A) -> (), @bas
  %z = alloc_var stack $A
  %x.arg = alloc_var stack $A
  copy_addr %x to initialize %x.arg
  apply %bas(%x.arg, %y, %z)
  dealloc_var stack %x.arg ; callee consumes %x.arg, caller deallocs
  ; ... use %z ...
  destroy_addr %z
  dealloc_var stack %z

The implementation of ``bas`` is then responsible for consuming ``%x.arg`` and
initializing ``%z``. In this trivial case, it could optimize down to a
take-initialization of the return value::
  
  func bas : $(*A, Int, *A) -> () {
  entry(%x, %y, %ret):
    copy_addr take %x to initialize %ret
    ret
  }

Tuple arguments are destructured recursively, regardless of the
address-only-ness of the tuple type. The destructured fields are passed
individually according to the above convention. This Swift function::

  struct [API] A {}

  func zim(x:Int, y:A, (z:Int, w:(A, Int)))

  zim(x, y, (z, w))

gets called in SIL as::

  %zim = constant_ref $(Int, *A, Int, *A, Int) -> (), @bas
  %y.arg = alloc_var stack $A
  copy_addr %y to initialize %y.arg
  %w.0 = element_addr %w, 0
  %w.0.arg = alloc_var stack $A
  copy_addr %w.0 to initialize %w.0.arg
  %w.1.addr = element_addr %w, 1
  %w.1 = load %w.1.addr
  apply %zim(%x, %y.arg, %z, %w.0.arg, %w.1)
  dealloc_var stack %w.0.arg
  dealloc_var stack %y.arg

Variadic arguments and tuple elements are packaged into an array and passed as
a single array argument. This Swift function::

  func zang(x:Int, (y:Int, z:Int...), v:Int, w:Int...)

  zang(x, (y, z0, z1), v, w0, w1, w2)

gets called in SIL as::

  %zang = constant_ref $(Int, Int, Int[], Int, Int[]) -> (), @zang
  %zs = <<make array from %z1, %z2>>
  %ws = <<make array from %w0, %w1, %w2>>
  apply %zang(%x, %y, %zs, %v, %ws)

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
