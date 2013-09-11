.. @raise litre.TestsAreMissing

Swift Intermediate Language (SIL)
=================================

.. contents::

Abstract
--------

SIL is an SSA-form IR with high-level semantic information designed to implement
the Swift programming language. SIL accommodates the following use cases:

- A set of guaranteed high-level optimizations that provide a predictable 
  baseline for runtime and diagnostic behavior.
- Diagnostic dataflow analysis passes that enforce Swift language requirements,
  such as definitive initialization of variables and constructors, code
  reachability, switch coverage.
- High-level optimization passes, including retain/release optimization,
  dynamic method devirtualization, closure inlining, memory allocation promotion, 
  and generic function instantiation.
- A stable distribution format that can be used to distribute "fragile"
  inlineable or generic code with Swift library modules, to be optimized into
  client binaries.

In contrast to LLVM IR, SIL is a generally target-independent format
representation that can be used for code distribution, but it can also express
target-specific concepts as well as Swift can.

SIL in the Swift Compiler
-------------------------

At a high level, the Swift compiler follows a strict pipeline architecture:

- The *Parse* module constructs an AST from Swift source code.
- The *Sema* module type-checks the AST and annotates it with type information.
- The *SILGen* module generates *raw SIL* from an AST.
- A series of *Guaranteed Optimization Passes* and *Diagnostic Passes* are run
  over the raw SIL to both perform optimizations, but also to emit
  language-specific diagnostics.  These are always run, even at -O0, and produce
  *canonical SIL*.
- General SIL *Optimization Passes* optionally run over the canonical SIL to
  improve performance of the resultant executable.  These are enabled and
  controlled by the optimization level and are not run at -O0.
- *IRGen* lowers optimized SIL to LLVM IR.
- The LLVM backend (optionally) applies LLVM optimizations, runs the LLVM code
  generator and emits binary code.

The stages pertaining to SIL processing in particular are as follows:

SILGen
~~~~~~

SILGen produces *raw SIL* by walking a type-checked Swift AST.
The form of SIL emitted by SILGen has the following properties:

- Variables are represented by loading and storing mutable memory locations
  instead of being in strict SSA form. This is similar to the initial
  ``alloca``-heavy LLVM IR emitted by frontends such as Clang. However, Swift
  represents variables as reference-counted "boxes" in the most general case,
  which can be retained, released, and captured into closures.
- Dataflow requirements, such as definitive assignment, function returns,
  switch coverage (TBD), etc. have not yet been enforced.
- ``always_inline``, ``always_instantiate``, and other function optimization
  attributes have not yet been honored.

These properties are addressed by subsequent guaranteed optimization and
diagnostic passes which are always run against the raw SIL.

Guaranteed Optimization and Diagnostic Passes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After SILGen, a deterministic sequence of optimization passes is run over the
raw SIL. We do not want the diagnostics produced by the compiler to change as
the compiler evolves, so these passes are intended to be simple and
predictable.

- **Mandatory inlining** inlines calls to "transparent" functions.
- **Memory promotion** is implemented as two optimization phases, the first
  of which performs capture analysis to promote ``alloc_box`` instructions to
  ``alloc_stack``, and the second of which promotes non-address-exposed ``alloc_stack``
  instructions to SSA registers.
- **Constant propagation** folds constant expressions and propagates the constant values. 
  If an arithmetic overflow occurs during the constant expression computation, a diagnostic 
  is issued.
- **Return analysis** verifies that each function returns a value on every
  code path and doesn't "fall of the end" of its definition, which is an error.
  It also issues an error when a ``noreturn`` function returns.

If all diagnostic passes succeed, the final result is the
*canonical SIL* for the program.

TODO:

- Generic specialization
- Basic ARC optimization for acceptable performance at -O0.

General Optimization Passes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

SIL captures language-specific type information, making it possible to
perform high-level optimizations—such as specialization of
generics—that are difficult to perform on LLVM IR.  The details of
these high-level optimizations have not been fully nailed down, but we
expect them to be important.

Syntax
------

SIL is reliant on Swift's type system and declarations, so SIL syntax
is an extension of Swift's. A ``.sil`` file is a Swift source file
with added SIL definitions. The Swift source is parsed only for its
declarations; Swift ``func`` bodies (except for nested declarations)
and top-level code are ignored by the SIL parser. In a ``.sil`` file,
there are no implicit imports; the ``swift`` and/or ``Builtin``
standard modules must be imported explicitly if used.

Here is an example of a ``.sil`` file::

  sil_stage canonical

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
    %4 = apply %1(%2, %3) : $(Double, Double) -> Double
    %5 = return %4 : Double
  }

SIL Stage
~~~~~~~~~
::

  sil-stage-decl ::= 'sil_stage' sil-stage

  sil-stage ::= 'raw'
  sil-stage ::= 'canonical'

There are different invariants on SIL depending on what stage of processing
has been applied to it.

* **Raw SIL** is the form produced by SILGen that has not been run through
  guaranteed optimizations or diagnostic passes. Raw SIL may not have a
  fully-constructed SSA graph. It may contain dataflow errors. Some instructions
  may be represented in non-canonical forms, such as ``assign`` and
  ``destroy_addr`` for non-address-only values. Raw SIL should not be used
  for native code generation or distribution.

* **Canonical SIL** is SIL as it exists after guaranteed optimizations and
  diagnostics. Dataflow errors must be eliminated, and certain instructions
  must be canonicalized to simpler forms. Performance optimization and native
  code generation are derived from this form, and a module can be distributed
  containing SIL in this (or later) forms.

SIL files declare the processing stage of the included SIL with one of the
declarations ``sil_stage raw`` or ``sil_stage canonical`` at top level. Only
one such declaration may appear in a file.

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
  as ``copy_addr`` or ``destroy_addr``, or as arguments to functions.
  For an address-only type ``T``, only the SIL address ``$*T`` can be
  formed, and ``$T`` is an invalid SIL type.
  
  Addresses are not reference-counted pointers like class values are. They
  cannot be retained or released.
  
  The address of an address cannot be taken. ``$**T`` is not a representable
  type. Values of address type thus cannot be allocated, loaded, or stored
  (though addresses can of course be loaded from and stored to).

  If a function takes address arguments, those addresses are assumed to be
  non-aliasing. A function may not capture an address, that is, it may not
  store the address value in a location that survives the duration of a
  function call. (Although addresses cannot directly be stored, they can be
  cast to ``Builtin.RawPointer`` values using the ``address_to_pointer``
  instruction, which could be stored.) In LLVM terms, all address arguments are
  ``noalias nocapture``. It is undefined behavior for two address arguments to
  alias or for a captured address value to be dereferenced.
  
  Functions cannot return an address. If an address-only
  value needs to be returned, it is done so using an indirect return argument
  according to the `calling convention`_ of the function.

- The *address of local storage for T* ``$*[local_storage] T``, a
  handle to a stack allocation of a variable of type ``$T``.

  For many types, the handle for a stack allocation is simply the
  allocated address itself.  However, if a type is runtime-sized, the
  compiler must emit code to potentially dynamically allocate memory.
  SIL abstracts over such differences by using values of local-storage
  type as the first result of ``alloc_stack`` and the operand of
  ``dealloc_stack``.

- Values of *generic function type* such as
  ``$<T...> (A...) -> R`` can be expressed in SIL.  Accessing a generic
  function with ``function_ref`` will give a value of a generic function type.
  Its type variables can be bound with a ``specialize`` instruction to
  give a value of a *concrete function type* ``$(A...) -> R``, which can then
  be applied. A generic function type value cannot be applied directly.

SIL classifies types into additional subgroups based on ABI stability and
generic constraints:

- *Loadable types* are types with a fully exposed concrete representation:

  * Reference types
  * Builtin value types
  * Fragile struct types in which all element types are loadable
  * Tuple types in which all element types are loadable
  * Class protocol types
  * Archetypes constrained by a class protocol

  A *loadable aggregate type* is a tuple or struct type that is loadable.

  A *trivial type* is a loadable type with trivial value semantics.
  Values of trivial type can be loaded and stored without any retain or
  release operations and do not need to be destroyed.

- *Runtime-sized types* are restricted value types for which the compiler
  does not know the size of the type statically:

  * Resilient value types
  * Fragile struct or tuple types that contain resilient types as elements at
    any depth
  * Archetypes not constrained by a class protocol

- *Address-only types* are restricted value types which cannot be
  loaded or otherwise worked with as SSA values:

  * Runtime-sized types
  * Non-class protocol types
  * [weak] types

  Values of address-only type (“address-only values”) must reside in
  memory and can only be referenced in SIL by address. Addresses of
  address-only values cannot be loaded from or stored to. SIL provides
  special instructions for indirectly manipulating address-only
  values, such as ``copy_addr`` and ``destroy_addr``.

Some additional meaningful categories of type:

- A *heap object reference* type is a type whose representation consists of a
  single strong-reference-counted pointer. This includes all class types,
  the ``Builtin.ObjectPointer`` and ``Builtin.ObjCPointer`` types, and
  archetypes that conform to one or more class protocols.
- A *reference type* is more general in that its low-level representation may
  include additional global pointers alongside a strong-reference-counted
  pointer. This includes all heap object reference types and adds
  thick function types and protocol/protocol composition types that conform to
  one or more class protocols. All reference types can be ``retain``-ed and
  ``release``-d. Reference types also have *ownership semantics* for their
  referenced heap object; see `Reference Counting`_ below.

SILGen does not always map Swift function types one-to-one to SIL function
types. Function types are transformed in order to encode additional attributes:

- The **calling convention** of the function, indicated by the

  .. parsed-literal::

    [cc(*convention*)]

  attribute—where *convention* can currently be ``swift``, ``method``,
  ``cdecl``, or ``objc``\ —describing a machine-level calling convention
  below the concern of SIL.

- The **thinness** of the function reference, indicated by the ``[thin]``
  attribute, which tracks whether a function reference requires a context value
  to reference captured closure state. Standalone functions and methods are
  always ``[thin]``, but function-local functions or closure expressions that
  capture context are thick. Partial applications of curried functions or
  methods are also thick.

- The **fully uncurried representation** of the function type, with
  all of the curried argument clauses flattened into a single argument
  clause. For instance, a curried function ``func foo(x:A)(y:B) -> C``
  might be emitted as a function of type ``((y:B), (x:A)) -> C``.  The
  exact representation depends on the function's `calling
  convention`_, which determines the exact ordering of currying
  clauses.  Methods are treated as a form of curried function.

TODO: Type-checking of cc and thin attributes will move into Swift's
type-checker and out of SIL eventually.

Values and Operands
~~~~~~~~~~~~~~~~~~~
::

  sil-identifier ::= [A-Za-z_0-9]+
  sil-value-name ::= '%' sil-identifier
  sil-value ::= sil-value-name ('#' [0-9]+)?
  sil-operand ::= sil-value ':' sil-type

SIL values are introduced with the ``%`` sigil and named by an
alphanumeric identifier, which references the instruction or basic block
argument that produces the value.
In SIL, a single instruction may produce multiple values. Operands that refer
to multiple-value instructions choose the value by following the ``%name`` with
``#`` and the index of the value. For example::

  // alloc_box produces two values--the refcounted pointer %box#0, and the
  // value address %box#1
  %box = alloc_box $Int64
  // Refer to the refcounted pointer
  %1 = strong_retain %box#0 : $Builtin.ObjectPointer
  // Refer to the address
  store %value to %box#1 : $*Int64

Unlike LLVM IR, SIL instructions that take value operands *only* accept
value operands. References to literal constants, functions, global variables, or
other entities require specialized instructions such as ``integer_literal``,
``function_ref``, ``global_addr``, etc.

Functions
~~~~~~~~~
::

  sil-function ::= 'sil' sil-linkage? sil-function-name ':' sil-type
                     '{' sil-basic-block+ '}'
  sil-function-name ::= '@' [A-Za-z_0-9]+

  sil-linkage ::= 'internal'
  sil-linkage ::= 'clang_thunk'

SIL functions are defined with the ``sil`` keyword. SIL function names
are introduced with the ``@`` sigil and named by an alphanumeric
identifier. This name will become the LLVM IR name for the function,
and is usually the mangled name of the originating Swift declaration.
The ``sil`` syntax declares the function's name and SIL type, and
defines the body of the function inside braces. The declared type must
be a function type, which may be generic.

The ``sil`` keyword may be optionally followed by a linkage specifier. By
default, SIL functions are externally visible from their enclosing module and
given LLVM ``external`` linkage.

- The ``internal`` specifier indicates that the function is internal
  to its module. Internal functions may be freely transformed by
  optimizations that might otherwise break code in other modules. Internal
  functions are given ``private`` linkage in LLVM IR.
- The ``clang_thunk`` specifier indicates that the function was generated as
  an adapter thunk to interface with a C or Objective-C declaration imported
  from Clang. These thunks are generated lazily and given ``linkonce_odr``
  linkage in LLVM IR.

Basic Blocks
~~~~~~~~~~~~
::

  sil-basic-block ::= sil-label sil-instruction-def* sil-terminator
  sil-label ::= sil-identifier ('(' sil-argument (',' sil-argument)* ')')? ':'
  sil-argument ::= sil-value-name ':' sil-type

  sil-instruction-def ::= (sil-value-name '=')? sil-instruction

A function body consists of one or more basic blocks that correspond
to the nodes of the function's control flow graph. Each basic block
contains one or more instructions and ends with a terminator
instruction. The function's entry point is always the first basic
block in its body.

In SIL, basic blocks take arguments, which are used as an alternative to LLVM's
phi nodes. Basic block arguments are bound by the branch from the predecessor
block::

  sil @iif : $(Builtin.Int1, Builtin.Int64, Builtin.Int64) -> Builtin.Int64 {
  bb0(%cond : $Builtin.Int1, %ifTrue : $Builtin.Int64, %ifFalse : $Builtin.Int64):
    condbranch %cond : $Builtin.Int1, then, else
  then:
    br finish(%ifTrue : $Builtin.Int64)
  else:
    br finish(%ifFalse : $Builtin.Int64)
  finish(%result : $Builtin.Int64):
    return %result : $Builtin.Int64
  }

Arguments to the entry point basic block, which has no predecessor,
are bound by the function's caller::

  sil @foo : $(Int) -> Int {
  bb0(%x : $Int):
    %1 = return %x : $Int
  }

  sil @bar : $(Int, Int) -> () {
  bb0(%x : $Int, %y : $Int):
    %foo = function_ref @foo
    %1 = apply %foo(%x) : $(Int) -> Int
    %2 = apply %foo(%y) : $(Int) -> Int
    %3 = tuple ()
    %4 = return %3 : $()
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
  sil-decl-subref-part ::= 'unionelt'
  sil-decl-subref-part ::= 'destroyer'
  sil-decl-subref-part ::= 'globalaccessor'
  sil-decl-subref-part ::= 'defaultarg' '.' [0-9]+
  sil-decl-uncurry-level ::= [0-9]+
  sil-decl-lang ::= 'objc'

Some SIL instructions need to reference Swift declarations directly. These
references are introduced with the ``#`` sigil followed by the fully qualified
name of the Swift declaration. Some Swift declarations are
decomposed into multiple entities at the SIL level. These are distinguished by
following the qualified name with ``!`` and one or more ``.``-separated component 
entity discriminators:

- ``getter``: the getter function for a ``var`` declaration
- ``setter``:  the setter function for a ``var`` declaration
- ``allocator``: a ``struct`` or ``union`` constructor, or a ``class``\ 's *allocating constructor*
- ``initializer``: a ``class``\ 's *initializing constructor*
- ``unionelt``: a member of a ``union`` type.
- ``destroyer``: a class's deallocating destructor
- ``globalaccessor``: the addressor function for a global variable
- ``defaultarg.``\ *n*: the default argument-generating function for
  the *n*\ -th argument of a Swift ``func``
- ``objc``: a specific entry point for objective-C interoperability

Methods and curried function definitions in Swift also have multiple
"uncurry levels" in SIL, representing the function at each possible
partial application level. For a curried function declaration::

  // Module example
  func foo(x:A)(y:B)(z:C) -> D

The declaration references and types for the different uncurry levels are as
follows::

  #example.foo!0 : $[thin] (x:A) -> (y:B) -> (z:C) -> D
  #example.foo!1 : $[thin] ((y:B), (x:A)) -> (z:C) -> D
  #example.foo!2 : $[thin] ((z:C), (y:B), (x:A)) -> D

The deepest uncurry level is referred to as the **natural uncurry level**.
Note that the uncurried argument clauses are composed right-to-left, as
specified in the `calling convention`_. For uncurry levels less than the
uncurry level, the entry point itself is ``[thin]`` but returns a thick
function value carrying the partially applied arguments for its context.

`Dynamic dispatch`_ instructions such as ``class method`` require their method
declaration reference to be uncurried to at least uncurry level 1 (which applies
both the "self" argument and the method arguments), because uncurry level zero
represents the application of the method to its "self" argument, as in
``foo.method``, which is where the dynamic dispatch semantically occurs
in Swift.

Dataflow Errors
---------------

*Dataflow errors* may exist in raw SIL. Swift's semantics defines these
conditions as errors, so they must be diagnosed by diagnostic
passes and must not exist in canonical SIL.

Definitive Initialization
~~~~~~~~~~~~~~~~~~~~~~~~~

Swift requires that all local variables be initialized before use. In
constructors, all instance variables of a struct, union, or class type must
be initialized before the object is used and before the constructor is returned
from.

Memory locations that require definitive initialization are currently modeled
using the `initialize_var`_ instruction. See the discussion below for more
details. ``initialize_var`` instructions that cannot be eliminated are dataflow
errors.

Unreachable Control Flow
~~~~~~~~~~~~~~~~~~~~~~~~

The ``unreachable`` terminator is emitted in raw SIL to mark incorrect control
flow, such as a non-``Void`` function failing to ``return`` a value, or a
``switch`` statement failing to cover all possible values of its subject.
The guaranteed dead code elimination pass can eliminate truly unreachable
basic blocks, or ``unreachable`` instructions may be dominated by applications
of ``[noreturn]`` functions. An ``unreachable`` instruction that survives
guaranteed DCE and is not immediately preceded by a ``[noreturn]``
application is a dataflow error.

Runtime Failure
---------------

Some operations, such as failed unconditional `checked conversions`_ or the
``Builtin.trap`` compiler builtin, cause a *runtime failure*, which
unconditionally terminates the current actor. If it can be proven that a
runtime failure will occur or did occur, runtime failures may be reordered so
long as they remain well-ordered relative to operations external to the actor
or the program as a whole. For instance, with overflow checking on integer
arithmetic enabled, a simple ``for`` loop that reads inputs in from one or more
arrays and writes outputs to another array, all local
to the current actor, may cause runtime failure in the update operations::

  // Given unknown start and end values, this loop may overflow
  for var i = unknownStartValue; i != unknownEndValue; ++i {
    ...
  }

It is permitted to hoist the overflow check and associated runtime failure out
of the loop itself and check the bounds of the loop prior to entering it, so
long as the loop body has no observable effect outside of the current actor.

Undefined Behavior
------------------

Incorrect use of some operations is *undefined behavior*, such as invalid
unchecked casts involving ``Builtin.RawPointer`` types, or use of compiler
builtins that lower to LLVM instructions with undefined behavior at the LLVM
level. A SIL program with undefined behavior is meaningless, much like undefined
behavior in C, and has no predictable semantics. Undefined behavior should not
be triggered by valid SIL emitted by a correct Swift program using a correct
standard library, but cannot in all cases be diagnosed or verified at the SIL
level.

Calling Convention
------------------

This section describes how Swift functions are emitted in SIL.

Swift Calling Convention [cc(swift)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Swift calling convention is the one used by default for native Swift
functions.

Tuples in the input type of the function are recursively destructured into
separate arguments, both in the entry point basic block of the callee, and
in the ``apply`` instructions used by callers::

  func foo(x:Int, y:Int)
  
  sil @foo : $(x:Int, y:Int) -> () {
  entry(%x : $Int, %y : $Int):
    ...
  }

  func bar(x:Int, y:(Int, Int))

  sil @bar : $(x:Int, y:(Int, Int)) -> () {
  entry(%x : $Int, %y0 : $Int, %y1 : $Int):
    ...
  }

  func call_foo_and_bar() {
    foo(1, 2)
    bar(4, (5, 6))
  }

  sil @call_foo_and_bar : $() -> () {
  entry:
    ...
    %foo = function_ref @foo : $(x:Int, y:Int) -> ()
    %foo_result = apply %foo(%1, %2) : $(x:Int, y:Int) -> ()
    ...
    %bar = function_ref @bar : $(x:Int, y:(Int, Int)) -> ()
    %bar_result = apply %bar(%4, %5, %6) : $(x:Int, y:(Int, Int)) -> ()
  }

Calling a function with trivial value types as inputs and outputs
simply passes the arguments by value. This Swift function::

  func foo(x:Int, y:Float) -> Char

  foo(x, y)

gets called in SIL as::

  %foo = constant_ref $(Int, Float) -> Char, @foo
  %z = apply %foo(%x, %y) : $(Int, Float) -> Char

Reference Counts
````````````````

Reference type arguments are passed in at +1 retain count and consumed by the
callee. A reference type return value is returned at +1 and consumed by the
caller. Value types with reference type components have their reference
type components each retained and released the same way. This Swift function::

  class A {}

  func bar(x:A) -> (Int, A) { ... }

  bar(x)

gets called in SIL as::

  %bar = function_ref @bar : $(A) -> (Int, A)
  strong_retain %x : $A
  %z = apply %bar(%x) : $(A) -> (Int, A)
  // ... use %z ...
  %z_1 = tuple_extract %z : $(Int, A), 1
  strong_release %z_1

When applying a thick function value as a callee, the function value is also
consumed at +1 retain count.

Address-Only Types
``````````````````

For address-only arguments, the caller allocates a copy and passes the address
of the copy to the callee. The callee takes ownership of the copy and is
responsible for destroying or consuming the value, though the caller must still
deallocate the memory. For address-only return values, the
caller allocates an uninitialized buffer and passes its address as the first
argument to the callee. The callee must initialize this buffer before
returning. This Swift function::

  struct [API] A {}

  func bas(x:A, y:Int) -> A { return x }

  var z = bas(x, y)
  // ... use z ...

gets called in SIL as::

  %bas = function_ref @bas : $(A, Int) -> A
  %z = alloc_stack $A
  %x_arg = alloc_stack $A
  copy_addr %x to [initialize] %x_arg : $*A
  apply %bas(%z, %x_arg, %y) : $(A, Int) -> A
  dealloc_stack %x_arg : $*A // callee consumes %x.arg, caller deallocs
  // ... use %z ...
  destroy_addr %z : $*A
  dealloc_stack stack %z : $*A

The implementation of ``@bas`` is then responsible for consuming ``%x_arg`` and
initializing ``%z``.

Tuple arguments are destructured regardless of the
address-only-ness of the tuple type. The destructured fields are passed
individually according to the above convention. This Swift function::

  struct [API] A {}

  func zim(x:Int, y:A, (z:Int, w:(A, Int)))

  zim(x, y, (z, w))

gets called in SIL as::

  %zim = function_ref @zim : $(x:Int, y:A, (z:Int, w:(A, Int))) -> ()
  %y_arg = alloc_stack $A
  copy_addr %y to [initialize] %y_arg : $*A
  %w_0_addr = element_addr %w : $*(A, Int), 0
  %w_0_arg = alloc_stack $A
  copy_addr %w_0_addr to [initialize] %w_0_arg : $*A
  %w_1_addr = element_addr %w : $*(A, Int), 1
  %w_1 = load %w_1_addr : $*Int
  apply %zim(%x, %y_arg, %z, %w_0_arg, %w_1) : $(x:Int, y:A, (z:Int, w:(A, Int))) -> ()
  dealloc_stack %w_0_arg
  dealloc_stack %y_arg

Variadic Arguments
``````````````````

Variadic arguments and tuple elements are packaged into an array and passed as
a single array argument. This Swift function::

  func zang(x:Int, (y:Int, z:Int...), v:Int, w:Int...)

  zang(x, (y, z0, z1), v, w0, w1, w2)

gets called in SIL as::

  %zang = function_ref @zang : $(x:Int, (y:Int, z:Int...), v:Int, w:Int...) -> ()
  %zs = <<make array from %z1, %z2>>
  %ws = <<make array from %w0, %w1, %w2>>
  apply %zang(%x, %y, %zs, %v, %ws)  : $(x:Int, (y:Int, z:Int...), v:Int, w:Int...) -> ()

Function Currying
`````````````````

Curried function definitions in Swift emit multiple SIL entry points, one for
each "uncurry level" of the function. When a function is uncurried, its
outermost argument clauses are combined into a tuple in right-to-left order.
For the following declaration::

  func curried(x:A)(y:B)(z:C)(w:D) -> Int {}

The types of the SIL entry points are as follows::

  sil @curried_0 : $(x:A) -> (y:B) -> (z:C) -> (w:D) -> Int { ... }
  sil @curried_1 : $((y:B), (x:A)) -> (z:C) -> (w:D) -> Int { ... }
  sil @curried_2 : $((z:C), (y:B), (x:A)) -> (w:D) -> Int { ... }
  sil @curried_3 : $((w:D), (z:C), (y:B), (x:A)) -> Int { ... }

Byref Arguments
```````````````

``[byref]`` arguments are passed into the entry point by address. The callee
does not take ownership of the referenced memory. The referenced memory must
be initialized upon function entry and exit. If the ``[byref]`` argument
refers to a fragile physical variable, then the argument is the address of that
variable. If the ``[byref]`` argument refers to a logical property, then the
argument is the address of a caller-owner writeback buffer. it is the caller's
responsibility to initialize the buffer by storing the result of the property
getter prior to calling the function and to write back to the property
on return by loading from the buffer and invoking the setter with the final
value. This Swift function::

  func byref(x:[byref] Int) {
    x = 1
  }

gets lowered to SIL as::

  sil @byref : $([byref] Int) -> () {
  entry(%x : $*Int):
    %1 = integer_literal 1 : $Int
    store %1 to %x
    return
  }

Swift Method Calling Convention [cc(method)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The method calling convention is currently identical to the freestanding
function convention. Methods are considered to be curried functions, taking
the "self" argument as their outer argument clause, and the method arguments
as the inner argument clause(s). When uncurried, the "self" argument is thus
passed last::

  struct Foo {
    func method(x:Int) -> Int {}
  }

  sil @Foo_method_1 : $((x : Int), [byref] Foo) -> Int { ... }

C Calling Convention [cc(cdecl)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Swift's C module importer, C types are always mapped to Swift types
considered trivial by SIL. SIL does not concern itself with platform
ABI requirements for indirect return, register vs. stack passing, etc.; C
function arguments and returns in SIL are always by value regardless of the
platform calling convention.

SIL (and therefore Swift) cannot currently invoke variadic C functions.

Objective-C Calling Convention [cc(objc)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Reference Counts
````````````````

Objective-C methods use the same argument and return value ownership rules as
ARC Objective-C. Selector families and the ``ns_consumed``,
``ns_returns_retained``, etc. attributes from imported Objective-C definitions
are honored.

Applying an ``[objc_block]`` value does not consume the block.

Method Currying
```````````````

In SIL, the "self" argument of an Objective-C method is uncurried to the last
argument of the uncurried type, just like a native Swift method.

  class [objc] NSString {
    func stringByPaddingToLength(Int) withString(NSString) startingAtIndex(Int)
  }

  sil @NSString_stringByPaddingToLength_withString_startingAtIndex \
    : $((Int, NSString, Int), NSString)

That ``self`` is passed as the first argument at the IR level is abstracted
away in SIL, as is the existence of the ``_cmd`` selector argument.

Instruction Set
---------------

Allocation and Deallocation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

These instructions allocate and deallocate memory.

alloc_stack
```````````
::

  sil-instruction ::= 'alloc_stack' sil-type

  %1 = alloc_stack $T
  // %1#0 has type $*[local_storage] T
  // %1#1 has type $*T

Allocates uninitialized memory that is sufficiently aligned on the stack
to contain a value of type ``T``. The first result of the instruction
is a local-storage handle suitable for passing to ``dealloc_stack``.
The second result of the instruction is the address of the allocated memory.

``alloc_stack`` marks the start of the lifetime of the value; the
allocation must be balanced with a ``dealloc_stack`` instruction to
mark the end of its lifetime.

The memory is not retainable; to allocate a retainable box for a value
type, use ``alloc_box``.

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
  //   %1#0 has type $Builtin.ObjectPointer
  //   %1#1 has type $*T

Allocates a reference-counted "box" on the heap large enough to hold a value of
type ``T``, along with a retain count and any other metadata required by the
runtime.  The result of the instruction is a two-value operand;
the first value is the reference-counted ``ObjectPointer`` that owns the box,
and the second value is the address of the value inside the box.

The box will be initialized with a retain count of 1; the storage will be
uninitialized. The box owns the contained value, and releasing it to a retain
count of zero destroys the contained value as if by ``destroy_addr``.
Releasing a box is undefined behavior if the box's value is uninitialized.
To deallocate a box whose value has not been initialized, ``dealloc_box``
should be used.

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
whose value has not been initialized, ``dealloc_box`` should be used.

dealloc_stack
`````````````
::

  sil-instruction ::= 'dealloc_stack' sil-operand

  dealloc_stack %0 : $*[local_storage] T
  // %0 must be of a local-storage $*[local_storage] T type

Deallocates memory previously allocated by ``alloc_stack``. The
allocated value in memory must be uninitialized or destroyed prior to
being deallocated. This instruction marks the end of the lifetime for
the value created by the corresponding ``alloc_stack`` instruction.

dealloc_box
```````````
::

  sil-instruction ::= 'dealloc_box' sil-type ',' sil-operand

  dealloc_box $Int, %0 : $Builtin.ObjectPointer

Deallocates a box, bypassing the reference counting mechanism. The box
variable must have a retain count of one. The boxed type must match the
type passed to the corresponding ``alloc_box`` exactly, or else
undefined behavior results.

This does not destroy the boxed value. The contents of the
value must have been fully uninitialized or destroyed before
``dealloc_box`` is applied.

dealloc_ref
```````````
::

  sil-instruction ::= 'dealloc_ref' sil-operand

  dealloc_ref %0 : $T
  // $T must be a class type

Deallocates a class type instance, bypassing the reference counting
mechanism. The instance must have a retain count of one. The type of
the operand must match the allocated type exactly, or else undefined
behavior results.

This does not destroy the reference type instance. The contents of the
heap object must have been fully uninitialized or destroyed before
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
value must be retained explicitly if necessary. It is undefined behavior to
load from uninitialized memory.

TODO: Should loading from uninitialized memory instead have the dataflow
analysis semantics of initialize_var?

store
`````
::

  sil-instruction ::= 'store' sil-value 'to' sil-operand

  store %0 to %1 : $*T
  // $T must be a loadable type

Stores the value ``%0`` to memory at address ``%1``.  The type of %1 is ``*T``
and the type of ``%0 is ``T``, which must be a loadable type. This will
overwrite the memory at ``%1``. If ``%1`` already references a value that
requires ``release`` or other cleanup, that value must be loaded before being
stored over and cleaned up.

assign
``````
::

  sil-instruction ::= 'assign' sil-value 'to' sil-operand

  assign %0 to %1 : $*T
  // $T must be a loadable type

Represents an abstract assignment of the value ``%0`` to memory at address
``%1`` without specifying whether it is an initialization or a normal store.
The type of %1 is ``*T`` and the type of ``%0`` is ``T``, which must be a
loadable type. This will overwrite the memory at ``%1`` and destroy the value
currently held there.

The purpose of the ``assign`` instruction is to simplify the
definitive initialization analysis on loadable variables by removing
what would otherwise appear to be a load and use of the current value.
It is produced by SILGen, which cannot know which assignments are
meant to be initializations.  If it is deemed to be an initialization,
it can be replaced with a ``store``; otherwise, it must be replaced
with a sequence that also correctly destroys the current value.

This instruction is only valid in Raw SIL and is rewritten as appropriate
by the definitive initialization pass.

initialize_var
``````````````
::

  sil-instruction ::= 'initialize_var' sil-operand

  initialize_var %0 : $*T
  // %0 must be an address $*T

TODO: Dataflow analysis not implemented yet. initialize_var currently is passed
through to IRGen and lowers to zero initialization.

TODO: Do we actually need an instruction to model this? Should alloc
instructions just implicitly require definitive initialization?

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

  sil-instruction ::= 'copy_addr' '[take]'? sil-value
                        'to' '[initialization]'? sil-operand

  copy_addr [take] %0 to [initialization] %1 : $*T
  // %0 and %1 must be of the same $*T address type

Loads the value at address ``%0`` from memory and assigns a copy of it back
into memory at address ``%1``. A bare ``copy_addr`` instruction::

  copy_addr %0 to %1 : $*T

is equivalent to::

  %new = load %0 : $*T        // Load the new value from the source
  %old = load %1 : $*T        // Load the old value from the destination
  strong_retain %new : $T            // Retain the new value
  strong_release %old : $T           // Release the old
  store %new to %1 : $*T      // Store the new value to the destination

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
    copy_addr [take] %0 to %1 : $*T
  // is equivalent to:
    %new = load %0 : $*T
    %old = load %1 : $*T
    // no retain of %new!
    strong_release %old : $T
    store %new to %1 : $*T

  // copy-initialization
    copy_addr %0 to [initialization] %1 : $*T
  // is equivalent to:
    %new = load %0 : $*T
    strong_retain %new : $T
    // no load/release of %old!
    store %new to %1 : $*T

  // take-initialization
    copy_addr [take] %0 to [initialization] %1 : $*T
  // is equivalent to:
    %new = load %0 : $*T
    // no retain of %new!
    // no load/release of %old!
    store %new to %1 : $*T

destroy_addr
````````````
::

  sil-instruction ::= 'destroy_addr' sil-operand

  destroy_addr %0 : $*T
  // %0 must be of an address $*T type

Destroys the value in memory at address ``%0``. This is equivalent to::

  %1 = load %0
  strong_release %1

except that ``destroy_addr`` may be used even if ``%0`` is of an
address-only type.  This does not deallocate memory; it only destroys the
pointed-to value, leaving the memory uninitialized.

index_addr
``````````
::

  sil-instruction ::= 'index_addr' sil-operand ',' sil-operand

  %2 = index_addr %0 : $*T, %1 : $Builtin.Int<n>
  // %0 must be of an address type $*T
  // %1 must be of a builtin integer type
  // %2 will be of type $*T

Given an address that references into an array of values, returns the address
of the ``%1``-th element relative to ``%0``. The address must reference into
a contiguous array, produced by ``alloc_array`` or by an external function. It
is undefined to try to reference offsets within a non-array value, such as
fields within a homogeneous struct or tuple type, or bytes within a value,
using ``index_addr``. (``Int8`` address types have no special behavior in this
regard, unlike ``char*`` or ``void*`` in C.) It is also undefined behavior to
index out of bounds of an array, except to index the "past-the-end" address of
the array.

index_raw_pointer
`````````````````
::

  sil-instruction ::= 'index_raw_pointer' sil-operand ',' sil-operand

  %2 = index_raw_pointer %0 : $Builtin.RawPointer, %1 : $Builtin.Int<n>
  // %0 must be of $Builtin.RawPointer type
  // %1 must be of a builtin integer type
  // %2 will be of type $*T

Given a ``Builtin.RawPointer`` value ``%0``, returns a pointer value at the
byte offset ``%1`` relative to ``%0``.

Reference Counting
~~~~~~~~~~~~~~~~~~

These instructions handle reference counting of heap objects. Values of
strong reference type have ownership semantics for the referenced heap
object. Retain and release operations, however,
are never implicit in SIL and always must be explicitly performed where needed.
Retains and releases on the value may be freely moved, and balancing
retains and releases may deleted, so long as an owning retain count is
maintained for the uses of the value.

All reference-counting operations are defined to work correctly on
null references (whether strong, unowned, or weak).  A non-null
reference must actually refer to a valid object of the indicated type
(or a subtype).  Address operands are required to be valid and non-null.

While SIL makes reference-counting operations explicit, the SIL type
system also fully represents strength of reference.  This is useful
for several reasons:

1. Type-safety: it is impossible to erroneously emit SIL that naively
   uses a ``[weak]`` or ``[unowned]`` reference as if it were a strong
   reference.

2. Consistency: when a reference is kept in memory, instructions like
   ``copy_addr`` and ``destroy_addr`` implicitly carry the right
   semantics in the type of the address, rather than needing special
   variants or flags.

3. Ease of tooling: SIL directly stores the user's intended strength
   of reference, making it straightforward to generate instrumentation
   that would convey this to a memory profiler.  In principle, with
   only a modest number of additions and restrictions on SIL, it would
   even be possible to drop all reference-counting instructions and
   use the type information to feed a garbage collector.

strong_retain
`````````````
::
  
  sil-instruction ::= 'strong_retain' sil-operand

  strong_retain %0 : $T
  // $T must be a reference type

Increases the strong retain count of the heap object referenced by ``%0``.

strong_retain_autoreleased
``````````````````````````
::

  sil-instruction ::= 'strong_retain_autoreleased' sil-operand

  strong_retain_autoreleased %0 : $T
  // $T must be a reference type

Retains the heap object referenced by ``%0`` using the Objective-C ARC
"autoreleased return value" optimization. The operand must be the result of an
``apply`` instruction with an Objective-C method callee, and the
``strong_retain_autoreleased`` instruction must be first use of the value after
the defining ``apply`` instruction.

TODO: Specify all the other strong_retain_autoreleased constraints here.

strong_release
``````````````
::

  strong_release %0 : $T
  // $T must be a reference type.

Decrements the strong reference count of the heap object referenced by ``%0``.
If the release operation brings the strong reference count of the object to
zero, the object is destroyed and ``[weak]`` references are cleared.  When both
its strong and unowned reference counts reach zero, the object's memory is
deallocated.

strong_retain_unowned
`````````````````````
::
  
  sil-instruction ::= 'strong_retain_unowned' sil-operand

  strong_retain_unowned %0 : $[unowned] T
  // $T must be a reference type

Asserts that the strong reference count of the heap object referenced by ``%0``
is still positive, then increases it by one.

ref_to_unowned
``````````````

::

  sil-instruction ::= 'ref_to_unowned' sil-operand

  %1 = unowned_to_ref %0 : T
  // $T must be a reference type
  // %1 will have type $[unowned] T

Adds the ``[unowned]`` qualifier to the type of a reference to a heap
object.  No runtime effect.

unowned_to_ref
``````````````

::

  sil-instruction ::= 'unowned_to_ref' sil-operand

  %1 = unowned_to_ref %0 : $[unowned] T
  // $T must be a reference type
  // %1 will have type $T

Strips the ``[unowned]`` qualifier off the type of a reference to a
heap object.  No runtime effect.

unowned_retain
``````````````
::
  
  sil-instruction ::= 'unowned_retain' sil-operand

  unowned_retain %0 : $[unowned] T
  // $T must be a reference type

Increments the unowned reference count of the heap object underlying ``%0``.

unowned_release
```````````````
::
  
  sil-instruction ::= 'unowned_release' sil-operand

  unowned_release %0 : $[unowned] T
  // $T must be a reference type

Decrements the unowned reference count of the heap object refereced by
``%0``.  When both its strong and unowned reference counts reach zero,
the object's memory is deallocated.

load_weak
`````````

::

  sil-instruction ::= 'load_weak' '[take]'? sil-operand

  load_weak [take] %0 : $*[weak] T
  // $T must be a reference type

Increments the strong reference count of the heap object held in the operand,
which must be an initialized weak reference.  The result is value of type
``$T``, except that it is ``null`` if the heap object has begun deallocation.

This operation must be atomic with respect to the final ``strong_release`` on
the operand heap object.  It need not be atomic with respect to ``store_weak``
operations on the same address.

store_weak
``````````

::

  sil-instruction ::= 'store_weak' sil-value 'to' '[initialization]'? sil-operand

  store_weak %0 to [initialization] %1 : $*[weak] T
  // $T must be a reference type

Initializes or reassigns a weak reference.  The operand may be ``null``.

If ``[initialization]`` is given, the weak reference must currently either be
uninitialized or destroyed.  If it is not given, the weak reference must
currently be initialized.

This operation must be atomic with respect to the final ``strong_release`` on
the operand (source) heap object.  It need not be atomic with respect to
``store_weak`` or ``load_weak`` operations on the same address.


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
``Builtin.Int<n>``, which must be a builtin integer type. The literal value
is specified using Swift's integer literal syntax.

float_literal
`````````````
::

  sil-instruction ::= 'float_literal' sil-type ',' int-literal

  %1 = float_literal $Builtin.FP<n>, 0x3F800000
  // $Builtin.FP<n> must be a builtin floating-point type
  // %1 has type $Builtin.FP<n>

Creates a floating-point literal value. The result will be of type ``
``Builtin.FP<n>``, which must be a builtin floating-point type. The literal
value is specified as the bitwise representation of the floating point value,
using Swift's hexadecimal integer literal syntax.

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
null-terminated. The string literal value is specified using Swift's string
literal syntax (though ``\()`` interpolations are not allowed).

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

TODO: ``builtin_zero`` is a temporary hack to support our current AST-level 
default initialization implementation. Definitive assignment analysis in SIL
will supersede this.

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
only be used as the callee of an ``apply`` instruction or ``partial_apply``
instruction. They cannot be stored or used as ``apply`` or ``partial_apply``
arguments.  ``objc_msgSend`` invocations must always be ``volatile``.

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
instance. It is undefined behavior if the class value is null and the
method is not an Objective-C method.

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
It is undefined behavior if the class value is null and the method is
not an Objective-C method.

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
  // If %0 is an address-only protocol address, then the "self" argument of
  //   the method type $[thin] U -> V must be $*P.This for #P.method's protocol
  //   of P
  // If %0 is a class protocol value, then the "self" argument of
  //   the method type $[thin] U -> V must be Builtin.ObjCPointer
  // If %0 is a protocol metatype, then the "self" argument of
  //   the method type $[thin] U -> V must be P.metatype

Looks up the implementation of a protocol method for the dynamic type of the
value inside an existential container. The "self" operand of the result
function value is represented using an opaque type, the value for which must
be projected out of the same existential container as the ``protocol_method``
operand:

- If the operand is the address of an address-only protocol type, then the
  "self" argument of the method is of type ``$*P.This``, the ``This`` archetype
  of the method's protocol.
- If the operand is a value of a class protocol type, then the "self"
  argument of the method is of type ``Builtin.ObjCPointer``, and can be
  projected using the ``project_existential_ref`` instruction.
- If the operand is a protocol metatype, it does not need to be projected, and
  the "self" argument of the method is the protocol metatype itself.

It is undefined behavior if the ``protocol_method`` function value is invoked
with a "self" argument not derived from the same existential container as the
method itself.

dynamic_method
```````````````
::

  sil-instruction ::= 'dynamic_method' sil-method-attributes?
                      sil-operand ',' sil-decl-ref ':' sil-type

  %1 = dynamic_method %0 : $P, #X.method!1 : $[thin] U -> V
  // %0 must be of a protocol or protocol composition type $P,
  // where $P contains the swift.DynamicLookup protocol
  // #X.method!1 must be a reference to an [objc] method of any class
  // or protocol type
  //
  // The "self" argument of the method type $[thin] U -> V must be 
  //   Builtin.ObjCPointer

Looks up the implementation of an Objective-C method with the same
selector as the named method for the dynamic type of the
value inside an existential container. The "self" operand of the result
function value is represented using an opaque type, the value for which must
be projected out as a value of type ``Builtin.ObjCPointer``.

It is undefined behavior if the dynamic type of the operand does not
have an implementation for the Objective-C method with the selector to
which the ``dynamic_method`` instruction refers, or if that
implementation has parameter or result types that are incompatible
with the method referenced by ``dynamic_method``. 
This instruction should only be used in cases where its result will be
immediately consumed by an operation that performs the selector check
itself (e.g., an ``apply`` that lowers to ``objc_msgSend``).
To query whether the operand has an implementation for the given
method and safely handle the case where it does not, use
`dynamic_method_br`_. 

Function Application
~~~~~~~~~~~~~~~~~~~~

These instructions call functions or wrap them in partial application or
specialization thunks.

apply
`````
::

  sil-instruction ::= 'apply' sil-value
                        '(' (sil-value (',' sil-value)*)? ')'
                        ':' sil-type

  %r = apply %0(%1, %2, ...) : $(A, B, ...) -> R
  // Note that the type of the callee '%0' is specified *after* the arguments
  // %0 must be of a concrete function type $(A, B, ...) -> R
  // %1, %2, etc. must be of the argument types $A, $B, etc.
  // %r will be of the return type $R

Transfers control to function ``%0``, passing it the given arguments. In
the instruction syntax, the type of the callee is specified after the argument
list; the types of the argument and of the defined value are derived from the
function type of the callee. The input argument tuple type is destructured,
and each element is passed as an individual argument. The ``apply``
instruction does no retaining or releasing of its arguments by itself; the
`calling convention`_'s retain/release policy must be handled by separate
explicit ``retain`` and ``release`` instructions. The return value will
likewise not be implicitly retained or released. ``%0`` must be an object of a
concrete function type; generic functions must have all of their generic
parameters bound with a ``specialize`` instruction before they can be applied.

NB: If the callee value is of a thick function type, ``apply`` currently
consumes the callee value at +1 strong retain count.

TODO: should have normal/unwind branch targets, like LLVM ``invoke``.

partial_apply
`````````````
::

  sil-instruction ::= 'partial_apply' sil-value
                        '(' (sil-value (',' sil-value)*)? ')'
                        ':' sil-type

  %c = partial_apply %0(%1, %2, ...) : $[thin] (T..., A, B, ...) -> R
  // Note that the type of the callee '%0' is specified *after* the arguments
  // %0 must be of a thin concrete function type $[thin] (T..., A, B, ...) -> R
  // %1, %2, etc. must be of the argument types $A, $B, etc.,
  //   of the tail part of the argument tuple of %0
  // %c will be of the partially-applied thick function type (T...) -> R

Creates a closure by partially applying the function ``%0`` to a partial
sequence of its arguments. In the instruction syntax, the type of the callee is
specified after the argument list; the types of the argument and of the defined
value are derived from the function type of the callee. The closure context will
be allocated with retain count 1 and initialized to contain the values ``%1``,
``%2``, etc.  The closed-over values will not be retained; that must be done
separately before the ``partial_apply``. The closure does take ownership of the
partially applied arguments; when the closure reference count reaches zero,
the contained values will be destroyed.

This instruction is used to implement both curry thunks and closures. A
curried function in Swift::

  func foo(a:A)(b:B)(c:C)(d:D) -> E { /* body of foo */ }

emits curry thunks in SIL as follows (retains and releases omitted for
clarity)::

  func @foo : $[thin] A -> B -> C -> D -> E {
  entry(%a : $A):
    %foo_1 = function_ref @foo_1 : $[thin] (B, A) -> C -> D -> E
    %thunk = partial_apply %foo_1(%a) : $[thin] (B, A) -> C -> D -> E
    return %thunk : $B -> C -> D -> E
  }

  func @foo_1 : $[thin] (B, A) -> C -> D -> E {
  entry(%b : $B, %a : $A):
    %foo_2 = function_ref @foo_2 : $[thin] (C, B, A) -> D -> E
    %thunk = partial_apply %foo_2(%b, %a) : $[thin] (C, B, A) -> D -> E
    return %thunk : $(B, A) -> C -> D -> E
  }

  func @foo_2 : $[thin] (C, B, A) -> D -> E {
  entry(%c : $C, %b : $B, %a : $A):
    %foo_3 = function_ref @foo_3 : $[thin] (D, C, B, A) -> E
    %thunk = partial_apply %foo_3(%c, %b, %a) : $[thin] (D, C, B, A) -> E
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
  
  func @bar : $[thin] (Int, Builtin.ObjectPointer, *Int) -> Int {
  entry(%y : $Int, %x_box : $Builtin.ObjectPointer, %x_address : $*Int):
    // ... body of bar ...
  }

  func @foo : $[thin] Int -> Int {
  entry(%x : $Int):
    // Create a box for the 'x' variable
    %x_box = alloc_box $Int
    store %x to %x_box#1 : $*Int

    // Create the bar closure
    %bar_uncurried = function_ref @bar : $(Int, Int) -> Int
    %bar = partial_apply %bar_uncurried(%x_box#0, %x_box#1) \
      : $(Int, Builtin.ObjectPointer, *Int) -> Int

    // Apply it
    %1 = integer_literal $Int, 1
    %ret = apply %bar(%1) : $(Int) -> Int

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
It is undefined behavior if the class instance reference is null.

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

If the destination type is a "simple" tuple type, that is, it has no keyword
argument labels or variadic arguments, then the first notation can be used,
which interleaves the element values and types. If keyword names or variadic
fields are specified, then the second notation must be used, which spells out
the tuple type before the fields.

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
variable inside the instance. It is undefined behavior if the class value
is null.

Unions
~~~~~~

These instructions construct values of union type. Loadable union values are
created with the ``union`` instruction. Address-only unions require two-step
initialization: first, if the case requires data, that data is stored into
the union at the address projected by ``union_data_addr``, then the tag for
the union is overlaid with an ``inject_union_addr`` instruction::

  union AddressOnlyUnion {
    case HasData(AddressOnlyType)
    case NoData
  }

  sil @init_with_data : $(AddressOnlyType) -> AddressOnlyUnion {
  entry(%0 : $*AddressOnlyUnion, %1 : $*AddressOnlyType):
    // Store the data argument for the case.
    %2 = union_data_addr %0 : $*AddressOnlyUnion, #AddressOnlyUnion.HasData
    copy_addr [take] %2 to [initialization] %1 : $*AddressOnlyType
    // Inject the tag.
    inject_union_addr %0 : $*AddressOnlyUnion, #AddressOnlyUnion.HasData
    return
  }

  sil @init_without_data : $() -> AddressOnlyUnion {
    // No data. We only need to inject the tag.
    inject_union_addr %0 : $*AddressOnlyUnion, #AddressOnlyUnion.NoData
    return
  }

Accessing the value of a union is tied to dispatching on its discriminator,
which is done with the ``switch_union`` and
``destructive_switch_union_addr`` `terminators`_.

union
`````
::

  sil-instruction ::= 'union' sil-type ',' sil-decl-ref (',' sil-operand)?

  %1 = union $U, #U.EmptyCase
  %1 = union $U, #U.DataCase, %0 : $T
  // $U must be a union type
  // #U.DataCase or #U.EmptyCase must be a case of union $U
  // If #U.Case has a data type $T, %0 must be a value of type $T
  // If #U.Case has no data type, the operand must be omitted
  // %1 will be of type $U

Creates a loadable union value in the given ``case``. If the ``case`` has a
data type, the union value will contain the operand value.

union_data_addr
```````````````
::

  sil-instruction ::= 'union_data_addr' sil-operand ',' sil-decl-ref

  %1 = union_data_addr %0 : $*U, #U.DataCase
  // $U must be a union type
  // #U.DataCase must be a case of union $U with data
  // %1 will be of address type $*T for the data type of case U.DataCase

Projects the address of the data for a union ``case`` inside a union. This
does not modify the union or check its value. It is intended to be used as
part of the initialization sequence for an address-only union. Storing to
the ``union_data_addr`` for a case followed by ``inject_union_addr`` with that
same case is guaranteed to result in a fully-initialized union value of that
case being stored. Loading from the ``union_data_addr`` of an initialized
union value or injecting a mismatched case tag is undefined behavior.

inject_union_addr
`````````````````
::

  sil-instruction ::= 'inject_union_addr' sil-operand ',' sil-decl-ref

  inject_union_addr %0 : $*U, #U.Case
  // $U must be a union type
  // #U.Case must be a case of union $U
  // %0 will be overlaid with the tag for #U.Case

Initializes the union value referenced by the given address by overlaying the
tag for the given case. If the case has no data, this instruction is sufficient
to initialize the union value. If the case has data, the data must be stored
into the union at the ``union_data_addr`` address for the case *before*
``inject_union_addr`` is applied. It is undefined behavior if
``inject_union_addr`` is applied for a case with data to an uninitialized union,
or if ``inject_union_addr`` is applied for a case with data when data for a
mismatched case has been stored to the union.

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
usual. It is undefined behavior to ``destroy_addr`` a partially-initialized
existential container.

upcast_existential
``````````````````
::

  sil-instruction ::= 'upcast_existential' '[take]'? sil-operand
                        'to' sil-operand

  upcast_existential %0 : $*protocol<P, Q> to %1 : $*P
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

  deinit_existential %0 : $*P
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

  sil-instruction ::= 'project_existential' sil-operand 'to' sil-type

  %1 = project_existential %0 : $*P to $*P.This
  // %0 must be of a $*P type for non-class protocol or protocol composition
  //   type P
  // $*P.This must be the address-of-This type for one of the protocols %0
  //   conforms to
  // %1 will be of type $*P.This

Obtains the address of the concrete value inside the
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
  apply %bar(%one_two_three, %foo_p) : $(Int, Builtin.OpaquePointer) -> ()

It is undefined behavior for the address to be passed as the
"self" argument to a method value obtained by ``protocol_method`` from
a different existential container. It is also undefined behavior if the
``OpaquePointer`` value is dereferenced, cast, or passed to a method after
the originating existential container has been mutated.

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
  apply %bar(%one_two_three, %foo_p) : $(Int, Builtin.ObjCPointer) -> ()

It is undefined behavior for the ``ObjCPointer`` value to be passed as the
"self" argument to a method value obtained by ``protocol_method`` from
a different existential container. It is also undefined behavior if the
``ObjCPointer`` value is dereferenced, cast, or passed to a method after the
originating existential container has been mutated.

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
  // The source and destination types must be exactly the same
  // %1 will have type $T

Represents a trivial type coercion. This instruction is emitted for source
fidelity to represent that an explicit ``x as T`` coercion was made; it has
no runtime effect. ``%1`` will be equivalent to ``%0``.

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
is undefined behavior to cast the ``RawPointer`` to any address type other than
its original address type.

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
is undefined behavior to cast the ``RawPointer`` back to any type other than
its original address type. It is also undefined behavior to cast a
``RawPointer`` from a heap object to any address type.

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
it is undefined behavior if the destination type is not a valid type for the
heap object.

ref_to_raw_pointer
``````````````````
::

  sil-instruction ::= 'ref_to_raw_pointer' sil-operand 'to' sil-type

  %1 = ref_to_raw_pointer %0 : $C to $Builtin.RawPointer
  // $C must be a class type, or Builtin.ObjectPointer, or Builtin.ObjCPointer
  // %1 will be of type $Builtin.RawPointer

Converts a heap object reference to a ``Builtin.RawPointer``. The ``RawPointer``
result can be cast back to the originating class type but does not have
ownership semantics. It is undefined behavior to cast a ``RawPointer`` from a
heap object reference to an address using ``pointer_to_address``.

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
semantics for the object on its own). It is undefined behavior to cast a
``RawPointer`` to a type unrelated to the dynamic type of the heap object.
It is also undefined behavior to cast a ``RawPointer`` from an address to any
heap object type.

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
in the following ways:

- Corresponding tuple elements may add, remove, or change keyword names.
  ``(a:Int, b:Float, Char) -> ()`` and ``(x:Int, Float, z:Char) -> ()`` are
  ABI compatible.
- A class tuple element of the destination type may be a superclass of the
  source type's corresponding tuple element.

The function types may also differ in attributes, with the following
exceptions:

- The ``cc``, ``thin``, and ``objc_block`` attributes cannot be changed.
- A ``[noreturn]`` function may be converted to a non-``[noreturn]``
  type, but a non-``[noreturn]`` function may not be converted to a
  ``[noreturn]`` function.

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
context information, into a thick function value with ignored context.
Applying the resulting thick function value is equivalent to applying the
original thin value. The ``thin_to_thick_function`` conversion may be
eliminated if the context is proven not to be needed.

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

is_nonnull
``````````
::

  sil-instruction ::= 'is_nonnull' sil-operand

  %1 = is_nonnull %0 : $C
  %1 = is_nonnull %0 : $*T
  // %0 must be of reference type $C or of address type $*T
  // %1 will be of type Builtin.Int1

Checks whether a reference type or address value is null, returning 1 if
the value is not null, or 0 if it is null.

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
  //   archetype $T (or a superclass of its superclass)
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

Terminators
~~~~~~~~~~~

These instructions terminate a basic block. Every basic block must end
with a terminator. Terminators may only appear as the final instruction of
a basic block.

unreachable
```````````
::
  
  sil-terminator ::= 'unreachable'

  unreachable

Indicates that control flow must not reach the end of the current basic block.
It is a dataflow error if an unreachable terminator is reachable from the entry
point of a function and is not immediately preceded by an ``apply`` of a
``[noreturn]`` function.

return
``````
::
  
  sil-terminator ::= 'return' sil-operand

  return %0 : $T
  // $T must be the return type of the current function

Exits the current function and returns control to the calling function. The
result of the ``apply`` instruction that invoked the current function will be
the operand of this ``return`` instruction.  ``return`` does not retain or
release its operand or any other values.

autorelease_return
``````````````````
::

  sil-terminator ::= 'autorelease_return' sil-operand

  autorelease_return %0 : $T
  // $T must be the return type of the current function, which must be of
  //   class type

Exits the current function and returns control to the calling function. The
result of the ``apply`` instruction that invoked the current function will be
the operand of this ``return`` instruction. The return value is autoreleased
into the active Objective-C autorelease pool using the "autoreleased return
value" optimization. The current function must use the ``[cc(objc)]`` calling
convention.

br
``
::

  sil-terminator ::= 'br' sil-identifier
                       '(' (sil-operand (',' sil-operand)*)? ')'

  br label (%0 : $A, %1 : $B, ...)
  // `label` must refer to a basic block label within the current function
  // %0, %1, etc. must be of the types of `label`'s arguments

Unconditionally transfers control from the current basic block to the block
labeled ``label``, binding the given values to the arguments of the destination
basic block.

condbranch
``````````
::

  sil-terminator ::= 'condbranch' sil-operand ','
                       sil-identifier '(' (sil-operand (',' sil-operand)*)? ')' ','
                       sil-identifier '(' (sil-operand (',' sil-operand)*)? ')'

  condbranch %0 : $Builtin.Int1, true_label (%a : $A, %b : $B, ...), \
                                 false_label (%x : $X, %y : $Y, ...)
  // %0 must be of $Builtin.Int1 type
  // `true_label` and `false_label` must refer to block labels within the
  //   current function
  // %a, %b, etc. must be of the types of `true_label`'s arguments
  // %x, %y, etc. must be of the types of `false_label`'s arguments

Conditionally branches to ``true_label`` if ``%0`` is equal to ``1`` or to
``false_label`` if ``%0`` is equal to ``0``, binding the corresponding set of
values to the the arguments of the chosen destination block.

switch_int
``````````
::

  sil-terminator ::= 'switch_int' sil-operand
                       (',' sil-switch-int-case)*
                       (',' sil-switch-default)?
  sil-switch-int-case ::= 'case' int-literal ':' sil-identifier
  sil-switch-default ::= 'default' sil-identifier

  switch_int %0 : $Builtin.Int<n>, case 1: label1, \
                                   case 2: label2, \
                                   ...,            \
                                   default labelN

  // %0 must be a value of builtin integer type $Builtin.Int<n>
  // `label1` through `labelN` must refer to block labels within the current
  //   function
  // FIXME: All destination labels currently must take no arguments

Conditionally branches to one of several destination basic blocks based on a
value of builtin integer type. If the operand value matches one of the ``case``
values of the instruction, control is transferred to the corresponding basic
block. If there is a ``default`` basic block, control is transferred to it if
the value does not match any of the ``case`` values. It is undefined behavior
if the value does not match any cases and no ``default`` branch is provided.

switch_union
````````````
::

  sil-terminator ::= 'switch_union' sil-operand
                       (',' sil-switch-union-case)*
                       (',' sil-switch-default)?
  sil-switch-union-case ::= 'case' sil-decl-ref ':' sil-identifier

  switch_union %0 : $U, case #U.Foo: label1, \
                        case #U.Bar: label2, \
                        ...,                 \
                        default labelN

  // %0 must be a value of union type $U
  // #U.Foo, #U.Bar, etc. must be 'case' declarations inside $U
  // `label1` through `labelN` must refer to block labels within the current
  //   function
  // label1 must take either no basic block arguments, or a single argument
  //   of the type of #U.Foo's data
  // label2 must take either no basic block arguments, or a single argument
  //   of the type of #U.Bar's data, etc.
  // labelN must take no basic block arguments

Conditionally branches to one of several destination basic blocks based on the
discriminator in a loadable ``union`` value. Unlike ``switch_int``,
``switch_union`` requires coverage of the operand type: If the ``union`` type
is resilient, the ``default`` branch is required; if the ``union`` type is
fragile, the ``default`` branch is required unless a destination is assigned to
every ``case`` of the ``union``. The destination basic block for a ``case`` may
take an argument of the corresponding ``union`` ``case``'s data type (or of the
address type, if the operand is an address). If the branch is taken, the
destination's argument will be bound to the associated data inside the
original union value.  For example::

  union Foo {
    case Nothing
    case OneInt(Int)
    case TwoInts(Int, Int)
  }

  sil @sum_of_foo : $Foo -> Int {
  entry(%x : $Foo):
    switch_union %x : $Foo,       \
      case #Foo.Nothing: nothing, \
      case #Foo.OneInt:  one_int, \
      case #Foo.TwoInts: two_ints

  nothing:
    %zero = integer_literal 0 : $Int
    return %zero : $Int

  one_int(%y : $Int):
    return %y : $Int

  two_ints(%ab : $(Int, Int)):
    %a = tuple_extract %ab : $(Int, Int), 0
    %b = tuple_extract %ab : $(Int, Int), 1
    %add = function_ref @add : $(Int, Int) -> Int
    %result = apply %add(%a, %b) : $(Int, Int) -> Int
    return %result : $Int
  }

destructive_switch_union_addr
`````````````````````````````
::

  sil-terminator ::= 'destructive_switch_union_addr' sil-operand
                       (',' sil-switch-union-case)*
                       (',' sil-switch-default)?

  destructive_switch_union_addr %0 : $*U, case #U.Foo: label1, \
                                          case #U.Bar: label2, \
                                          ...,                 \
                                          default labelN

  // %0 must be the address of a union type $*U
  // #U.Foo, #U.Bar, etc. must be cases of $U
  // `label1` through `labelN` must refer to block labels within the current
  //   function
  // label1 must take a single argument of the address type of #U.Foo's data
  // label2 must take a single argument of the address type of #U.Bar's data
  // labelN must take no basic block arguments

Conditionally branches to one of several destination basic blocks based on
the discriminator in the union value referenced by the address operand.
If a case is matched by the switch, the union value is destructured in-place,
invalidating the union value, and the address of the data for the matched case
is passed to the destination basic block as an argument. Destroying the
data is guaranteed equivalent to destroying the original value.
Destroying the original union after it has been successfully matched by a case
is undefined behavior.  In the default case, the union is left unmodified.

Unlike ``switch_int``, ``switch_union`` requires coverage of the operand type:
If the ``union`` type is resilient, the ``default`` branch is required; if the
``union`` type is fragile, the ``default`` branch is required unless a
destination is assigned to every ``case`` of the ``union``.

dynamic_method_br
`````````````````
::

  sil-terminator ::= 'dynamic_method_br' sil-operand ',' sil-decl-ref 
                       ',' sil-identifier ',' sil-identifier

  dynamic_method_br %0 : $P, #X.method!1, bb1, bb2
  // %0 must be of type Builtin.ObjCPointer 
  // where $P contains the swift.DynamicLookup protocol
  // #X.method!1 must be a reference to an [objc] method of any class
  // or protocol type

Looks up the implementation of an Objective-C method with the same
selector as the named method for the dynamic type of the value inside
an existential container. The "self" operand of the result function
value is represented using an opaque type, the value for which must be
projected out as a value of type ``Builtin.ObjCPointer``.

If the operand is determined to have the named method, this
instruction branches to ``bb1``, passing it the uncurried function
corresponding to the method found. If the operand does not have the
named method, this instruction branches to ``bb2``.
