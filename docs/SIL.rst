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

- Memory promotion: this is implemented as two optimization phases, the first
  of which performs capture analysis to promote alloc_box instructions to
  alloc_stack, and the second of which promotes non-address-exposed alloc_stack
  instructions to SSA registers.

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

Accessing Memory
~~~~~~~~~~~~~~~~

load
````
::

  sil-instruction ::= 'load' sil-operand

  %1 = load %0 : $*T
  // %0 must be of a $*T address type for loadable type $T
  // %1 will be of type $T

Loads the value at address ``%0`` from memory. ``T`` must be a loadable type.
This does not affect the reference count, if any, of the loaded value; the
value must be retained explicitly if necessary.

store
`````
::

  store %0 : $T to %1 : $*T
  // $T must be a loadable type

Stores the value ``%0`` to memory at address ``%1``. ``%0`` must be of a
loadable type. This will overwrite the memory at ``%1``; ``%1`` must point
to uninitialized or destroyed memory.

initialize_var
``````````````
::

  initialize_var %0 : $*T
  // %0 must be an address $*T

TODO: Dataflow analysis not implemented yet. initialize_var currently is passed
through to IRGen and lowers to zero initialization.

TODO: Do we actually need an instruction to model this?

A pseudo-instruction that notionally "stores" the "must be initialized" value
to the address ``%0``. In dataflow analysis, this value has the following
semantics:

- It can be loaded but not stored. If it is of an address-only type,
  ``copy_addr`` cannot use its address as a source. ``destroy_addr``
  may take its address as an operand; it is a no-op.
- A "must be initialized" value cannot be used as the argument of an ``apply``
  or ``partial_apply`` instruction, and cannot be used as part of a ``struct``
  or ``tuple`` construction.
- Retaining and releasing the value, or any part of the value, is a no-op.
- Extracting or projecting any component of the value, as by
  ``struct_extract``, ``tuple_extract``, ``project_existential``, etc.,
  produces another "must be initialized" value (or the address of such a value).
- The address containing the value can be overwritten as the destination of a 
  ``store`` or ``copy_addr``. A ``copy_addr`` assignment can be promoted to a
  ``copy_addr`` ``[initialization]``.

The goal of these semantics is model definitive assignment, that is, the
requirement that local variables and instance variable fields be initialized
before use. Dataflow analysis verifies these semantics then eliminates the
instruction.

copy_addr
`````````
::

  sil-instruction ::= 'copy_addr' '[take]'? sil-operand
                        'to' '[initialization]'? sil-operand

  %_ = copy_addr [take] %0 : $*T to [initialization] %1 : $*T
  // %0 and %1 must be of the same $*T address type

Loads the value at address ``%0`` from memory and assigns a copy of it back
into memory at address ``%1``. A bare ``copy_addr`` instruction::

  copy_addr %0 : $*T to %1 : $*T

is equivalent to::

  %new = load %0 : $*T        // Load the new value from the source
  %old = load %1 : $*T        // Load the old value from the destination
  retain %new : $T            // Retain the new value
  release %old : $T           // Release the old
  store %new : $T to %1 : $*T // Store the new value to the destination

except that ``copy_addr`` may be used even if ``%0`` is of an address-only
type. The ``copy`` may be given one or both of the ``[take]`` or
``[initialization]`` attributes:

* ``[take]`` destroys the value at the source address in the course of the
  copy.
* ``[initialization]`` indicates that the destination address is uninitialized.
  Without the attribute, the destination address is treated as already
  initialized, and the existing value will be destroyed before the new value
  is stored.

The three attributed forms thus behave like the following loadable type
operations::

  // take-assignment
    copy_addr [take] %0 : $*T to %1 : $*T
  // is equivalent to:
    %new = load %0 : $*T
    %old = load %1 : $*T
    // no retain of %new!
    release %old : $T
    store %new : $T to %1 : $*T

  // copy-initialization
    copy_addr %0 : $*T to [initialization] %1 : $*T
  // is equivalent to:
    %new = load %0 : $*T
    retain %new : $T
    // no load/release of %old!
    store %new : $T to %1 : $*T

  // take-initialization
    copy_addr [take] %0 : $*T to [initialization] %1 : $*T
  // is equivalent to:
    %new = load %0 : $*T
    ; no retain of %new!
    ; no load/release of %old!
    store %new : $T to %1 : $*T

destroy_addr
````````````
::

  sil-instruction ::= 'destroy_addr' sil-operand

  %_ = destroy_addr %0 : $*T
  // %0 must be of an address $*T type

Destroys the value in memory at address ``%0``. This is equivalent to::

  %1 = load %0
  release %1

except that ``destroy_addr`` may be used even if ``%0`` is of an
address-only type.  This does not deallocate memory; it only destroys the
pointed-to value, leaving the memory uninitialized.

Reference Counting
~~~~~~~~~~~~~~~~~~

These instructions handle reference counting of heap objects.

retain
``````
::
  
  sil-instruction ::= 'retain' sil-operand

  %_ = retain %0 : $T
  // %0 must be of a reference type

Retains the heap object referenced by ``%0``.

retain_autoreleased
```````````````````
::

  sil-instruction ::= 'retain_autoreleased' sil-operand

  %_ = retain_autoreleased %0 : $T
  // %0 must be of a reference type

Retains the heap object referenced by ``%0`` using the Objective-C ARC
"autoreleased return value" optimization. The operand must be the result of
an ``apply`` instruction with an Objective-C method callee, and the
``retain_autoreleased`` instruction must be first use of the value after the
defining ``apply`` instruction.

TODO: Specify all the other retain_autoreleased constraints here.

release
```````
::

  %_ = release %0
  // %0 must be of a reference type.

Releases the heap object referenced by ``%0``. If the release
operation brings the retain count of the object to zero, the object
is destroyed and its memory is deallocated.

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

module
``````
::

  sil-instruction ::= 'module' sil-decl-ref

  %1 = module #M
  // #M must be a module name
  // %1 has type $module<M>

Creates a module value for the module ``M``.

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
instance. The reference must not be null.

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
The class reference must not be null.

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

TODO: Partial application of already thick functions should be supported but
is not implemented.

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
The class instance reference must not be null.

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

Aggregate Types
~~~~~~~~~~~~~~~

These instructions construct and project elements from structs, tuples, and
class instances.

tuple
`````
::
  
  sil-instruction ::= 'tuple' sil-tuple-elements
  sil-tuple-elements ::= '(' (sil-operand (',' sil-operand)*)? ')'
  sil-tuple-elements ::= sil-type '(' (sil-value (',' sil-value)*)? ')'

  %1 = tuple (%a : $A, %b : $B, ...)
  // $A, $B, etc. must be loadable non-address types
  // %1 will be of the "simple" tuple type $(A, B, ...)

  %1 = tuple $(a:A, b:B, ...) (%a, %b, ...)
  // (a:A, b:B, ...) must be a loadable tuple type
  // %1 will be of the type $(a:A, b:B, ...)

Creates a loadable tuple value by aggregating multiple loadable values.

If the
destination type is a "simple" tuple type, that is, it has no keyword argument
labels or variadic arguments, then the first notation can be used, which
interleaves the element values and types. If keyword names or variadic fields
are specified, then the second notation must be used, which spells out the
tuple type before the fields.

tuple_extract
`````````````
::

  sil-instruction ::= 'tuple_extract' sil-operand ',' int-literal

  %1 = tuple_extract %0 : $(T...), 123
  // %0 must be of a loadable tuple type $(T...)
  // %1 will be of the type of the selected element of %0

Extracts an element from a loadable tuple value.

tuple_element_addr
``````````````````
::

  sil-instruction ::= 'tuple_element_addr' sil-operand ',' int-literal

  %1 = tuple_element_addr %0 : $*(T...), 123
  // %0 must of a $*(T...) address-of-tuple type
  // %1 will be of address type $*U where U is the type of the 123rd
  //   element of T

Given the address of a tuple in memory, derives the
address of an element within that value.

struct
``````
::

  sil-instruction ::= 'struct' sil-type '(' (sil-operand (',' sil-operand)*)? ')'

  %1 = struct $S (%a : $A, %b : $B, ...)
  // $S must be a loadable struct type
  // $A, $B, ... must be the types of the physical 'var' fields of $S in order
  // %1 will be of type $S

Creates a value of a loadable struct type by aggregating multiple loadable
values.

struct_extract
``````````````
::

  sil-instruction ::= 'struct_extract' sil-operand ',' sil-decl-ref

  %1 = struct_extract %0 : $S, #S.field
  // %0 must be of a loadable struct type $S
  // #S.field must be a physical 'var' field of $S
  // %1 will be of the type of the selected field of %0

Extracts a physical field from a loadable struct value.

struct_element_addr
```````````````````
::

  sil-instruction ::= 'struct_element_addr' sil-operand ',' sil-decl-ref

  %1 = struct_element_addr %0 : $*S, #S.field
  // %0 must be of a struct type $S
  // #S.field must be a physical 'var' field of $S
  // %1 will be the address of the selected field of %0

Given the address of a struct value in memory, derives the address of a
physical field within the value.

ref_element_addr
````````````````
::

  sil-instruction ::= 'ref_element_addr' sil-operand ',' sil-decl-ref

  %1 = ref_element_addr %0 : $C, #C.field
  // %0 must be a value of class type $C
  // #C.field must be a non-static physical field of $C
  // %1 will be of type $*U where U is the type of the selected field
  //   of C

Given an instance of a class, derives the address of a physical instance
variable inside the instance. The class reference must not be null.

Protocol and Protocol Composition Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These instructions create and manipulate values of protocol and protocol
composition type.  From SIL's perspective, protocol and protocol composition
types consist of an *existential container*, which is a generic container for
a value of unknown runtime type, referred to as an "existential type" in type
theory. The existential container consists of a reference to the
*witness table(s)* for the protocol(s) referred to by the protocol type and a
reference to the underlying *concrete value*, which may be either stored
in-line inside the existential container for small values or allocated
separately into a buffer owned and managed by the existential container for
larger values.

If none of the protocols in a protocol type are class protocols, then
the existential container for that type is address-only and referred to in
the implementation as an *opaque existential container*. The value semantics of
the existential container propagate to the contained concrete value. Applying
``copy_addr`` to an opaque existential container copies the
contained concrete value, deallocating or reallocating the destination
container's owned buffer if necessary. Applying ``destroy_addr`` to an
opaque existential container destroys the concrete value and deallocates any
buffers owned by the existential container.

If a protocol type is constrained by one or more class protocols, then the
existential container for that type is loadable and referred to in the
implementation as a *class existential container*. Class existential containers
have reference semantics and can be ``retain``-ed and ``release``-d.

init_existential
````````````````
::
  
  sil-instruction ::= 'init_existential' sil-operand ',' sil-type

  %1 = init_existential %0 : $*P, $T
  // %0 must be of a $*P address type for non-class protocol or protocol
  //   composition type P
  // $T must be a type that fulfills protocol(s) P
  // %1 will be of type $*T

Partially initializes the memory referenced by ``%0`` with an existential
container prepared to contain a value of type ``$T``. The result of the
instruction is an address referencing the storage for the contained value, which
remains uninitialized. The contained value must be ``store``-d or
``copy_addr``-ed to in order for the existential value to be fully initialized.
If the existential container needs to be destroyed while the contained value
is uninitialized, ``deinit_existential`` must be used to do so. A fully
initialized existential container can be destroyed with ``destroy_addr`` as
usual.

upcast_existential
``````````````````
::

  sil-instruction ::= 'upcast_existential' '[take]'? sil-operand
                        'to' sil-operand

  %_ = upcast_existential %0 : $*protocol<P, Q> to %1 : $*P
  // %0 must be the address of a non-class protocol or protocol composition
  //   type
  // %1 must be the address of a non-class protocol or protocol composition
  //   type that is a supertype of %0

Initializes the memory referenced by the destination ``%1`` with the value
contained in the existing existential container referenced by ``%0``. 
The ``[take]`` attribute may be applied to the instruction, in which case,
the source existential container is destroyed and ownership of the contained
value is taken by the destination. Without the ``[take]`` attribute, the
destination receives an independently-owned copy of the value.

deinit_existential
``````````````````
::

  sil-instruction ::= 'deinit_existential' sil-operand

  %_ = deinit_existential %0 : $*P
  // %0 must be of a $*P address type for non-class protocol or protocol
  // composition type P

Undoes the partial initialization performed by
``init_existential``.  ``deinit_existential`` is only valid for
existential containers that have been partially initialized by
``init_existential`` but haven't had their contained value initialized.
A fully initialized existential must be destroyed with ``destroy_addr``.

project_existential
```````````````````
::

  sil-instruction ::= 'project_existential' sil-operand

  %1 = project_existential %0 : $*P
  // %0 must be of a $*P type for non-class protocol or protocol composition
  //   type P
  // %1 will be of type $Builtin.OpaquePointer

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

compiles to this SIL sequence::

  // ... initialize %foo
  %bar = protocol_method %foo : $*Foo, #Foo.bar!1
  %foo_p = project_existential %foo : $*Foo
  %one_two_three = integer_literal $Int, 123
  %_ = apply %bar(%one_two_three : $Int, %foo_p : $Builtin.OpaquePointer) : $(Int, Builtin.OpaquePointer) -> ()

It is invalid for the result of ``project_existential`` is used as anything
other than the "this" argument of an instance method reference obtained by
``protocol_method`` from the same existential container.

init_existential_ref
````````````````````
::

  sil-instruction ::= 'init_existential_ref' sil-operand ',' sil-type

  %1 = init_existential_ref %0 : $C, $P
  // %0 must be of class type $C conforming to protocol(s) $P
  // $P must be a class protocol or protocol composition type
  // %1 will be of type $P

Creates a class existential container of type ``$P`` containing a reference to
the class instance ``%0``.

upcast_existential_ref
``````````````````````
::

  sil-instruction ::= 'upcast_existential_ref' sil-operand 'to' sil-type

  %1 = upcast_existential_ref %0 : $protocol<P, Q> to $P
  // %0 must be of a class protocol or protocol composition type
  // $P must be a class protocol or protocol composition type that is a
  //   supertype of %0's type

Converts a class existential container to a more general protocol or protocol
composition type.

project_existential_ref
```````````````````````
::

  sil-instruction ::= 'project_existential_ref' sil-operand

  %1 = project_existential_ref %0 : $P
  // %0 must be of a class protocol or protocol composition type $P
  // %1 will be of type $Builtin.ObjCPointer

Extracts the class instance reference from a class existential container as a
``Builtin.ObjCPointer``. This value can be passed to protocol instance methods
obtained by ``protocol_method`` from the same existential container. A method
call on a class-protocol-type value in Swift::

  protocol [class_protocol] Foo {
    func bar(x:Int)
  }

  var foo:Foo
  // ... initialize foo
  foo.bar(123)

compiles to this SIL sequence::

  // ... initialize %foo
  %bar = protocol_method %foo : $Foo, #Foo.bar!1
  %foo_p = project_existential_ref %foo : $Foo
  %one_two_three = integer_literal $Int, 123
  %_ = apply %bar(%one_two_three : $Int, %foo_p : $Builtin.ObjCPointer) : $(Int, Builtin.ObjCPointer) -> ()

It is invalid for the result of ``project_existential_ref`` is used as anything
other than the "this" argument of an instance method reference obtained by
``protocol_method`` from the same existential container.

Unchecked Conversions
~~~~~~~~~~~~~~~~~~~~~

These instructions implement type conversions which are not checked. These are
either user-level conversions that are always safe and do not need to be
checked, or implementation detail conversions that are unchecked for
performance or flexibility.

coerce
``````
::

  sil-instruction ::= 'coerce' sil-operand 'to' sil-type

  %1 = coerce %0 : $T to $T
  // %1 will have type $T

Represents an explicit type coercion with no runtime effect. ``%1`` will be
equivalent to ``%0``.

upcast
``````
::

  sil-instruction ::= 'upcast' sil-operand 'to' sil-type

  %1 = coerce %0 : $D to $B
  // $D and $B must be class types or metatypes, with B a superclass of D
  // %1 will have type $B

Represents a conversion from a derived class instance or metatype to a
superclass.

archetype_ref_to_super
``````````````````````
::

  sil-instruction ::= 'archetype_ref_to_super' sil-operand 'to' sil-type

  %1 = archetype_to_super %0 : $T to $B
  // %0 must be of an archetype type $T with a base class constraint
  // $B must be the base class constraint type of $T or a superclass thereof
  // %1 will be of the base type $B

Represents a conversion from a generic type to a superclass specified as a
constraint of the generic type.

address_to_pointer
``````````````````
::

  sil-instruction ::= 'address_to_pointer' sil-operand 'to' sil-type

  %1 = address_to_pointer %0 : $*T to $Builtin.RawPointer
  // %0 must be of an address type $*T
  // %1 will be of type Builtin.RawPointer

Creates a ``Builtin.RawPointer`` value corresponding to the address ``%0``.
Converting the result pointer back to an address of the same type will give
an address equivalent to ``%0``. Type punning is always undefined in SIL; it
is invalid to cast the ``RawPointer`` back to any type other than its
original address type.

pointer_to_address
``````````````````
::

  sil-instruction ::= 'pointer_to_address' sil-operand 'to' sil-type

  %1 = pointer_to_address %0 : $Builtin.RawPointer to $*T
  // %1 will be of type $*T

Creates an address value corresponding to the ``Builtin.RawPointer`` value
``%0``.  Converting a ``RawPointer`` back to an address of the same type as
its originating ``address_to_pointer`` instruction gives back an equivalent
address. Type punning is always undefined in SIL; it
is invalid to cast the ``RawPointer`` back to any type other than its
original address type. It is also invalid to cast a ``RawPointer`` from a
heap object to any address type. This conversion, however, is unchecked and
will not raise a compile-time or runtime error if used incorrectly.

ref_to_object_pointer
`````````````````````
::

  sil-instruction ::= 'ref_to_object_pointer' sil-operand 'to' sil-type

  %1 = ref_to_object_pointer %0 : $C to $Builtin.ObjectPointer
  // %0 must be of class type $C
  // %1 will be of type $Builtin.ObjectPointer

Converts a class instance reference to the ``Builtin.ObjectPointer`` type.

object_pointer_to_ref
`````````````````````
::

  sil-instruction ::= 'object_pointer_to_ref' sil-operand 'to' sil-type

  %1 = object_pointer_to_ref %0 : $Builtin.ObjectPointer to $C
  // $C must be a class type
  // %1 will be of type $C

Converts a ``Builtin.ObjectPointer`` value to a class instance reference.
The destination type ``$C`` must be the correct type (or a superclass) of the
type of the referenced heap object. This conversion, however, is unchecked and
will not raise a compile-time or runtime error if used incorrectly.

ref_to_raw_pointer
``````````````````
::

  sil-instruction ::= 'ref_to_raw_pointer' sil-operand 'to' sil-type

  %1 = ref_to_raw_pointer %0 : $C to $Builtin.RawPointer
  // $C must be a class type, or Builtin.ObjectPointer, or Builtin.ObjCPointer
  // %1 will be of type $Builtin.RawPointer

Converts a heap object reference to a ``Builtin.RawPointer``. The ``RawPointer``
result can be cast back to the originating class type but does not have
ownership semantics. It is invalid to cast a ``RawPointer`` from a heap
object reference to an address using ``pointer_to_address``.

raw_pointer_to_ref
``````````````````
::
  
  sil-instruction ::= 'raw_pointer_to_ref' sil-operand 'to' sil-type

  %1 = raw_pointer_to_ref %0 : $Builtin.RawPointer to $C
  // $C must be a class type, or Builtin.ObjectPointer, or Builtin.ObjCPointer
  // %1 will be of type $C

Converts a ``Builtin.RawPointer`` back to a heap object reference. Casting
a heap object reference to ``Builtin.RawPointer`` back to the same type gives
an equivalent heap object reference (though the raw pointer has no ownership
semantics for the object on its own). It is invalid to cast a ``RawPointer`` to
a type unrelated to the dynamic type of the referenced object. It is invalid
to cast a ``RawPointer`` from an address to any heap object type. The
conversion, however, is unchecked and will not raise a compile-time or runtime
error if used incorrectly.

convert_function
````````````````
::

  sil-instruction ::= 'convert_function' sil-operand 'to' sil-type

  %1 = convert_function %0 : $T -> U to $T' -> U'
  // %0 must be of a function type $T -> U ABI-compatible with $T' -> U'
  //   (see below)
  // %1 will be of type $T' -> U'

Performs a conversion of the function ``%0`` to type ``T``, which must be ABI-
compatible with the type of ``%0``. Function types are ABI-compatible if their
input and result types are tuple types that, after destructuring, differ only
in label names or default values.

convert_cc
``````````
::

  sil-instruction ::= 'convert_cc' sil-operand 'to' sil-type

  %1 = convert_cc %0 : $[cc(X)] T -> U to $[cc(Y)] T -> U
  // %0 must be of a function type
  // The destination must be the same function type, differing only in
  //   calling convention
  // %1 will be of type $[cc(Y)] T -> U

Thunks the calling convention of a function. If the input operand is statically
a ``function_ref`` instruction, the result can be ``[thin]``; otherwise, the
result must be thick.

bridge_to_block
```````````````
::

  sil-instruction ::= 'bridge_to_block' sil-operand 'to' sil-type

  %1 = bridge_to_block %0 : $T -> U to $[cc(cdecl), objc_block] T -> U
  // %0 must be of a function type
  // The destination must be of the same function type, with the 
  //   [objc_block] attribute
  // %1 will be of type $[cc(cdecl), objc_block] T -> U

Converts a function value from Swift representation to Objective-C block
representation.

thin_to_thick_function
``````````````````````
::

  sil-instruction ::= 'thin_to_thick_function' sil-operand 'to' sil-type

  %1 = thin_to_thick_function %0 : $[thin] T -> U to $T -> U
  // %0 must be of a thin function type $[thin] T -> U
  // The destination type must be the corresponding thick function type
  // %1 will be of type $T -> U

Converts a thin function value, that is, a bare function pointer with no
context information, into a thick function value with empty context.

Checked Conversions
~~~~~~~~~~~~~~~~~~~

These instructions represent user-level cast operations that can fail and thus
require runtime checking. All of these instructions take a flag to indicate
the desired behavior of the runtime check::

  sil-checked-conversion-mode ::= 'conditional'
  sil-checked-conversion-mode ::= 'unconditional'

- ``conditional`` causes the conversion to return a null address or reference
  if the cast fails. The success of the conversion be tested with
  the ``is_nonnull`` instruction.
- ``unconditional`` requires the conversion to succeed. It is a runtime failure
  if the cast fails.

downcast
````````
::

  sil-instruction ::= 'downcast' sil-checked-conversion-mode 
                        sil-operand 'to' sil-type

  %1 = downcast conditional conditional %0 : $B to $D
  // %0 must be of a class type $B that is a superclass of $D
  // $D must be a class type
  // %1 will be of type $D

Performs a checked downcast conversion of class instance reference ``%0`` to
a subclass ``D`` of its current static type.

super_to_archetype_ref
``````````````````````
::

  sil-instruction :: 'super_to_archetype_ref' sil-checked-conversion-mode
                       sil-operand 'to' sil-type

  %1 = super_to_archetype_ref conditional %0 : $B to $T
  // %0 must be of a class type $B that is the superclass constraint of
  //  archetype $T (or a superclass of its superclass)
  // %1 will be of type $T

Performs a checked downcast operation on the class instance reference ``%0``
to an archetype ``T`` constrained by the class type.

downcast_archetype_ref
``````````````````````
::

  sil-instruction :: 'downcast_archetype_ref' sil-checked-conversion-mode
                       sil-operand 'to' sil-type

  %1 = downcast_archetype_ref conditional %0 : $T to $A
  // %0 must be of a class archetype $T
  // $A must be a concrete class type or another class archetype
  // %1 will be of type $A

Performs a checked conversion of a class instance from a class archetype to a
concrete class type or to another archetype.

downcast_archetype_addr
```````````````````````
::

  sil-instruction :: 'downcast_archetype_addr' sil-checked-conversion-mode
                       sil-operand 'to' sil-type

  %1 = downcast_archetype_ref conditional %0 : $*T to $*A
  // %0 must be the address of an archetype $*T
  // $*A must the address of a concrete type or of another archetype
  // %1 will be of type $*A

Performs a checked conversion of an address from an archetype to a concrete
class type or to another archetype.

project_downcast_existential_addr
`````````````````````````````````
::

  sil-instruction ::= 'project_downcast_existential_addr'
                        sil-checked-conversion-mode
                        sil-operand 'to' sil-type

  %1 = project_downcast_existential_addr conditional %0 : $*P to $*A
  // %0 must be the address of an opaque existential container $*P
  // $*A must the address of a concrete type or archetype
  // %1 will be of type $*A

Performs a checked conversion on the value inside of an opaque existential
container. If the conversion succeeds, the address of the contained value is
projected out of the existential container.

downcast_existential_ref
````````````````````````
::

  sil-instruction ::= 'downcast_existential_ref' sil-checked-conversion-mode
                        sil-operand 'to' sil-type

  %1 = downcast_existential_ref conditional %0 : $P to $C
  // %0 must be a class existential container value of type $P
  // $C must be a concrete class type or class archetype
  // %1 will be of type $C

Performs a checked conversion on the class instance reference inside of a
class existential container. If the conversion succeeds, the contained
class instance is returned.

is_nonnull
``````````
::

  sil-instruction ::= 'is_nonnull' sil-operand

  %1 = is_nonnull %0 : $C
  %1 = is_nonnull %0 : $*T
  // %0 must be of reference type $C or of address type $*T
  // %1 will be of type swift.Bool

TODO: The instruction should produce a Builtin.i1 and we should emit a
conversion to swift.Bool when needed.

Checks whether a reference type or address value is null, returning true if
the value is not null, or false if it is null.

Array Indexing
~~~~~~~~~~~~~~

TODO

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

TODO design questions
---------------------

* debug information representation
* maintaining good AST location info in the face of optimization
