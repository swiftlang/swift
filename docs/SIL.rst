.. @raise litre.TestsAreMissing
.. highlight:: none

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
target-specific concepts as well as LLVM can.

SIL in the Swift Compiler
-------------------------

At a high level, the Swift compiler follows a strict pipeline architecture:

- The *Parse* module constructs an AST from Swift source code.
- The *Sema* module type-checks the AST and annotates it with type information.
- The *SILGen* module generates *raw SIL* from an AST.
- A series of *Guaranteed Optimization Passes* and *Diagnostic Passes* are run
  over the raw SIL both to perform optimizations and to emit
  language-specific diagnostics.  These are always run, even at -Onone, and
  produce *canonical SIL*.
- General SIL *Optimization Passes* optionally run over the canonical SIL to
  improve performance of the resulting executable.  These are enabled and
  controlled by the optimization level and are not run at -Onone.
- *IRGen* lowers canonical SIL to LLVM IR.
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
- ``transparent`` function optimization has not yet been honored.

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
  code path and doesn't "fall off the end" of its definition, which is an error.
  It also issues an error when a ``noreturn`` function returns.
- **Critical edge splitting** splits all critical edges from terminators that
  don't support arbitrary basic block arguments (all non cond_branch
  terminators).

If all diagnostic passes succeed, the final result is the
*canonical SIL* for the program.

TODO:

- Generic specialization
- Basic ARC optimization for acceptable performance at -Onone.

General Optimization Passes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

SIL captures language-specific type information, making it possible to
perform high-level optimizations that are difficult to perform on LLVM
IR.

- **Generic Specialization** analyzes specialized calls to generic
  functions and generates new specialized version of the
  functions. Then it rewrites all specialized usages of the generic
  to a direct call of the appropriate specialized function.
- **Witness and VTable Devirtualization** for a given type looks up
  the associated method from a class's vtable or a type witness table
  and replaces the indirect virtual call with a call to the mapped
  function.
- **Performance Inlining**
- **Reference Counting Optimizations**
- **Memory Promotion/Optimizations**
- **High-level domain specific optimizations** The Swift compiler implements
  high-level optimizations on basic Swift containers such as Array or String.
  Domain specific optimizations require a defined interface between
  the standard library and the optimizer. More details can be found here:
  :ref:`HighLevelSILOptimizations`

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

  import Swift

  // Define types used by the SIL function.

  struct Point {
    var x : Double
    var y : Double
  }

  class Button {
    func onClick()
    func onMouseDown()
    func onMouseUp()
  }

  // Declare a Swift function. The body is ignored by SIL.
  func taxicabNorm(_ a:Point) -> Double {
    return a.x + a.y
  }

  // Define a SIL function.
  // The name @_T5norms11taxicabNormfT1aV5norms5Point_Sd is the mangled name
  // of the taxicabNorm Swift function.
  sil @_T5norms11taxicabNormfT1aV5norms5Point_Sd : $(Point) -> Double {
  bb0(%0 : $Point):
    // func Swift.+(Double, Double) -> Double
    %1 = function_ref @_Tsoi1pfTSdSd_Sd
    %2 = struct_extract %0 : $Point, #Point.x
    %3 = struct_extract %0 : $Point, #Point.y
    %4 = apply %1(%2, %3) : $(Double, Double) -> Double
    %5 = return %4 : Double
  }

  // Define a SIL vtable. This matches dynamically-dispatched method
  // identifiers to their implementations for a known static class type.
  sil_vtable Button {
    #Button.onClick!1: @_TC5norms6Button7onClickfS0_FT_T_
    #Button.onMouseDown!1: @_TC5norms6Button11onMouseDownfS0_FT_T_
    #Button.onMouseUp!1: @_TC5norms6Button9onMouseUpfS0_FT_T_
  }

SIL Stage
~~~~~~~~~
::

  decl ::= sil-stage-decl
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

SIL types are introduced with the ``$`` sigil. SIL's type system is
closely related to Swift's, and so the type after the ``$`` is parsed
largely according to Swift's type grammar.

Type Lowering
`````````````

A *formal type* is the type of a value in Swift, such as an expression
result.  Swift's formal type system intentionally abstracts over a
large number of representational issues like ownership transfer
conventions and directness of arguments.  However, SIL aims to
represent most such implementation details, and so these differences
deserve to be reflected in the SIL type system.  *Type lowering* is
the process of turning a formal type into its *lowered type*.

It is important to be aware that the lowered type of a declaration
need not be the lowered type of the formal type of that declaration.
For example, the lowered type of a declaration reference:

- will usually be thin,

- will frequently be uncurried,

- may have a non-Swift calling convention,

- may use bridged types in its interface, and

- may use ownership conventions that differ from Swift's default
  conventions.

Abstraction Difference
``````````````````````

Generic functions working with values of unconstrained type must
generally work with them indirectly, e.g. by allocating sufficient
memory for them and then passing around pointers to that memory.
Consider a generic function like this:

::

  func generateArray<T>(n : Int, generator : () -> T) -> [T]

The function ``generator`` will be expected to store its result
indirectly into an address passed in an implicit parameter.  There's
really just no reasonable alternative when working with a value of
arbitrary type:

- We don't want to generate a different copy of ``generateArray`` for
  every type ``T``.

- We don't want to give every type in the language a common
  representation.

- We don't want to dynamically construct a call to ``generator``
  depending on the type ``T``.

But we also don't want the existence of the generic system to force
inefficiencies on non-generic code.  For example, we'd like a function
of type ``() -> Int`` to be able to return its result directly; and
yet, ``() -> Int`` is a valid substitution of ``() -> T``, and a
caller of ``generateArray<Int>`` should be able to pass an arbitrary
``() -> Int`` in as the generator.

Therefore, the representation of a formal type in a generic context
may differ from the representation of a substitution of that formal type.
We call such differences *abstraction differences*.

SIL's type system is designed to make abstraction differences always
result in differences between SIL types.  The goal is that a properly-
abstracted value should be correctly usable at any level of substitution.

In order to achieve this, the formal type of a generic entity should
always be lowered using the abstraction pattern of its unsubstituted
formal type.  For example, consider the following generic type:

::

  struct Generator<T> {
    var fn : () -> T
  }
  var intGen : Generator<Int>

``intGen.fn`` has the substituted formal type ``() -> Int``, which
would normally lower to the type ``@callee_owned () -> Int``, i.e.
returning its result directly.  But if that type is properly lowered
with the pattern of its unsubstituted type ``() -> T``, it becomes
``@callee_owned () -> @out Int``.

When a type is lowered using the abstraction pattern of an
unrestricted type, it is lowered as if the pattern were replaced with
a type sharing the same structure but replacing all materializable
types with fresh type variables.

For example, if ``g`` has type ``Generator<(Int, Int) -> Float>``, ``g.fn`` is
lowered using the pattern ``() -> T``, which eventually causes ``(Int, Int)
-> Float`` to be lowered using the pattern ``T``, which is the same as
lowering it with the pattern ``U -> V``; the result is that ``g.fn``
has the following lowered type::

  @callee_owned () -> @owned @callee_owned (@in (Int, Int)) -> @out Float.

As another example, suppose that ``h`` has type
``Generator<(Int, inout Int) -> Float>``.  Neither ``(Int, inout Int)``
nor ``inout Int`` are potential results of substitution because they
aren't materializable, so ``h.fn`` has the following lowered type::

  @callee_owned () -> @owned @callee_owned (@in Int, @inout Int) -> @out Float

This system has the property that abstraction patterns are preserved
through repeated substitutions.  That is, you can consider a lowered
type to encode an abstraction pattern; lowering ``T`` by ``R`` is
equivalent to lowering ``T`` by (``S`` lowered by ``R``).

SILGen has procedures for converting values between abstraction
patterns.

At present, only function and tuple types are changed by abstraction
differences.

Legal SIL Types
```````````````

The type of a value in SIL shall be:

- a loadable legal SIL type, ``$T``,

- the address of a legal SIL type, ``$*T``, or

A type ``T`` is a *legal SIL type* if:

- it is a function type which satisfies the constraints (below) on
  function types in SIL,

- it is a metatype type which describes its representation,

- it is a tuple type whose element types are legal SIL types,

- it is ``Optional<U>``, where ``U`` is a legal SIL type,

- it is a legal Swift type that is not a function, tuple, optional,
  metatype, or l-value type, or

- it is a ``@box`` containing a legal SIL type.

Note that types in other recursive positions in the type grammar are
still formal types.  For example, the instance type of a metatype or
the type arguments of a generic type are still formal Swift types, not
lowered SIL types.

Address Types
`````````````

The *address of T* ``$*T`` is a pointer to memory containing a value
of any reference or value type ``$T``.  This can be an internal
pointer into a data structure. Addresses of loadable types can be
loaded and stored to access values of those types.

Addresses of address-only types (see below) can only be used with
instructions that manipulate their operands indirectly by address, such
as ``copy_addr`` or ``destroy_addr``, or as arguments to functions.
It is illegal to have a value of type ``$T`` if ``T`` is address-only.

Addresses are not reference-counted pointers like class values are. They
cannot be retained or released.

Address types are not *first-class*: they cannot appear in recursive
positions in type expressions.  For example, the type ``$**T`` is not
a legal type.

The address of an address cannot be directly taken. ``$**T`` is not a representable
type. Values of address type thus cannot be allocated, loaded, or stored
(though addresses can of course be loaded from and stored to).

Addresses can be passed as arguments to functions if the corresponding
parameter is indirect.  They cannot be returned.

Box Types
`````````

Captured local variables and the payloads of ``indirect`` value types are stored
on the heap. The type ``@box T`` is a reference-counted type that references
a box containing a mutable value of type ``T``. Boxes always use Swift-native
reference counting, so they can be queried for uniqueness and cast to the
``Builtin.NativeObject`` type.

Metatype Types
``````````````

A concrete or existential metatype in SIL must describe its representation.
This can be:

- ``@thin``, meaning that it requires no storage and thus necessarily
  represents an exact type (only allowed for concrete metatypes);

- ``@thick``, meaning that it stores a reference to a type or (if a
  concrete class) a subclass of that type; or

- ``@objc``, meaning that it stores a reference to a class type (or a
  subclass thereof) using an Objective-C class object representation
  rather than the native Swift type-object representation.

Function Types
``````````````

Function types in SIL are different from function types in Swift in a
number of ways:

- A SIL function type may be generic.  For example, accessing a
  generic function with ``function_ref`` will give a value of
  generic function type.

- A SIL function type declares its conventional treatment of its
  context value:

  - If it is ``@convention(thin)``, the function requires no context value.

  - If it is ``@callee_owned``, the context value is treated as an
    owned direct parameter.

  - If it is ``@callee_guaranteed``, the context value is treated as
    a guaranteed direct parameter.

  - Otherwise, the context value is treated as an unowned direct
    parameter.

- A SIL function type declares the conventions for its parameters.
  The parameters are written as an unlabeled tuple; the elements of that
  tuple must be legal SIL types, optionally decorated with one of the
  following convention attributes.

  The value of an indirect parameter has type ``*T``; the value of a
  direct parameter has type ``T``.

  - An ``@in`` parameter is indirect.  The address must be of an
    initialized object; the function is responsible for destroying
    the value held there.

  - An ``@inout`` parameter is indirect.  The address must be of an
    initialized object. The memory must remain initialized for the duration
    of the call until the function returns. The function may mutate the
    pointee, and furthermore may weakly assume that there are no aliasing
    reads from or writes to the argument, though must preserve a valid
    value at the argument so that well-ordered aliasing violations do not
    compromise memory safety. This allows for optimizations such as local
    load and store propagation, introduction or elimination of temporary
    copies, and promotion of the ``@inout`` parameter to an ``@owned`` direct
    parameter and result pair, but does not admit "take" optimization out
    of the parameter or other optimization that would leave memory in an
    uninitialized state.

  - An ``@inout_aliasable`` parameter is indirect. The address must be of an
    initialized object. The memory must remain initialized for the duration
    of the call until the function returns. The function may mutate the
    pointee, and must assume that other aliases may mutate it as well. These
    aliases however can be assumed to be well-typed and well-ordered; ill-typed
    accesses and data races to the parameter are still undefined.

  - An ``@owned`` parameter is an owned direct parameter.

  - A ``@guaranteed`` parameter is a guaranteed direct parameter.

  - An ``@in_guaranteed`` parameter is indirect.  The address must be of an
    initialized object; both the caller and callee promise not to mutate the
    pointee, allowing the callee to read it.

  - Otherwise, the parameter is an unowned direct parameter.

- A SIL function type declares the conventions for its results.
  The results are written as an unlabeled tuple; the elements of that
  tuple must be legal SIL types, optionally decorated with one of the
  following convention attributes.  Indirect and direct results may
  be interleaved.

  Indirect results correspond to implicit arguments of type ``*T`` in
  function entry blocks and in the arguments to ``apply`` and ``try_apply``
  instructions.  These arguments appear in the order in which they appear
  in the result list, always before any parameters.

  Direct results correspond to direct return values of type ``T``.  A
  SIL function type has a ``return type`` derived from its direct results
  in the following way: when there is a single direct result, the return
  type is the type of that result; otherwise, it is the tuple type of the
  types of all the direct results, in the order they appear in the results
  list.  The return type is the type of the operand of ``return``
  instructions, the type of ``apply`` instructions, and the type of
  the normal result of ``try_apply`` instructions.

  - An ``@out`` result is indirect.  The address must be of an
    uninitialized object.  The function is required to leave an
    initialized value there unless it terminates with a ``throw``
    instruction or it has a non-Swift calling convention.

  - An ``@owned`` result is an owned direct result.

  - An ``@autoreleased`` result is an autoreleased direct result.
    If there is an autoreleased result, it must be the only direct result.

  - Otherwise, the parameter is an unowned direct result.

A direct parameter or result of trivial type must always be unowned.

An owned direct parameter or result is transferred to the recipient,
which becomes responsible for destroying the value. This means that
the value is passed at +1.

An unowned direct parameter or result is instantaneously valid at the
point of transfer.  The recipient does not need to worry about race
conditions immediately destroying the value, but should copy it
(e.g. by ``strong_retain``\ ing an object pointer) if the value will be
needed sooner rather than later.

A guaranteed direct parameter is like an unowned direct parameter
value, except that it is guaranteed by the caller to remain valid
throughout the execution of the call. This means that any
``strong_retain``, ``strong_release`` pairs in the callee on the
argument can be eliminated.

An autoreleased direct result must have a type with a retainable
pointer representation.  Autoreleased results are nominally transferred
at +0, but the runtime takes steps to ensure that a +1 can be safely
transferred, and those steps require precise code-layout control.
Accordingly, the SIL pattern for an autoreleased convention looks exactly
like the SIL pattern for an owned convention, and the extra runtime
instrumentation is inserted on both sides when the SIL is lowered into
LLVM IR.  An autoreleased ``apply`` of a function that is defined with
an autoreleased result has the effect of a +1 transfer of the result.
An autoreleased ``apply`` of a function that is not defined with
an autoreleased result has the effect of performing a strong retain in
the caller.  A non-autoreleased ``apply`` of a function that is defined
with an autoreleased result has the effect of performing an
autorelease in the callee.

- The @noescape declaration attribute on Swift parameters (which is valid only
  on parameters of function type, and is implied by the @autoclosure attribute)
  is turned into a @noescape type attribute on SIL arguments.  @noescape
  indicates that the lifetime of the closure parameter will not be extended by
  the callee (e.g. the pointer will not be stored in a global variable).  It
  corresponds to the LLVM "nocapture" attribute in terms of semantics (but is
  limited to only work with parameters of function type in Swift).

- SIL function types may provide an optional error result, written by
  placing ``@error`` on a result.  An error result is always
  implicitly ``@owned``.  Only functions with a native calling
  convention may have an error result.

  A function with an error result cannot be called with ``apply``.
  It must be called with ``try_apply``.
  There is one exception to this rule: a function with an error result can be
  called with ``apply [nothrow]`` if the compiler can prove that the function
  does not actually throw.

  ``return`` produces a normal result of the function.  To return
  an error result, use ``throw``.

  Type lowering lowers the ``throws`` annotation on formal function
  types into more concrete error propagation:

  - For native Swift functions, ``throws`` is turned into an error
    result.

  - For non-native Swift functions, ``throws`` is turned in an
    explicit error-handling mechanism based on the imported API.  The
    importer only imports non-native methods and types as ``throws``
    when it is possible to do this automatically.

Properties of Types
```````````````````

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
  * @weak types

  Values of address-only type ("address-only values") must reside in
  memory and can only be referenced in SIL by address. Addresses of
  address-only values cannot be loaded from or stored to. SIL provides
  special instructions for indirectly manipulating address-only
  values, such as ``copy_addr`` and ``destroy_addr``.

Some additional meaningful categories of type:

- A *heap object reference* type is a type whose representation consists of a
  single strong-reference-counted pointer. This includes all class types,
  the ``Builtin.NativeObject`` and ``Builtin.UnknownObject`` types, and
  archetypes that conform to one or more class protocols.
- A *reference type* is more general in that its low-level representation may
  include additional global pointers alongside a strong-reference-counted
  pointer. This includes all heap object reference types and adds
  thick function types and protocol/protocol composition types that conform to
  one or more class protocols. All reference types can be ``retain``-ed and
  ``release``-d. Reference types also have *ownership semantics* for their
  referenced heap object; see `Reference Counting`_ below.
- A type with *retainable pointer representation* is guaranteed to
  be compatible (in the C sense) with the Objective-C ``id`` type.
  The value at runtime may be ``nil``.  This includes classes,
  class metatypes, block functions, and class-bounded existentials with
  only Objective-C-compatible protocol constraints, as well as one
  level of ``Optional`` or ``ImplicitlyUnwrappedOptional`` applied to any of the
  above.  Types with retainable pointer representation can be returned
  via the ``@autoreleased`` return convention.

SILGen does not always map Swift function types one-to-one to SIL function
types. Function types are transformed in order to encode additional attributes:

- The **convention** of the function, indicated by the

  .. parsed-literal::

    @convention(*convention*)

  attribute. This is similar to the language-level ``@convention``
  attribute, though SIL extends the set of supported conventions with
  additional distinctions not exposed at the language level:

  - ``@convention(thin)`` indicates a "thin" function reference, which uses
    the Swift calling convention with no special "self" or "context" parameters.
  - ``@convention(thick)`` indicates a "thick" function reference, which
    uses the Swift calling convention and carries a reference-counted context
    object used to represent captures or other state required by the function.
  - ``@convention(block)`` indicates an Objective-C compatible block reference.
    The function value is represented as a reference to the block object,
    which is an ``id``-compatible Objective-C object that embeds its invocation
    function within the object. The invocation function uses the C calling
    convention.
  - ``@convention(c)`` indicates a C function reference. The function value
    carries no context and uses the C calling convention.
  - ``@convention(objc_method)`` indicates an Objective-C method implementation.
    The function uses the C calling convention, with the SIL-level ``self``
    parameter (by SIL convention mapped to the final formal parameter)
    mapped to the ``self`` and ``_cmd`` arguments of the implementation.
  - ``@convention(method)`` indicates a Swift instance method implementation.
    The function uses the Swift calling convention, using the special ``self``
    parameter.
  - ``@convention(witness_method)`` indicates a Swift protocol method
    implementation. The function's polymorphic convention is emitted in such
    a way as to guarantee that it is polymorphic across all possible
    implementors of the protocol.

- The **fully uncurried representation** of the function type, with
  all of the curried argument clauses flattened into a single argument
  clause. For instance, a curried function ``func foo(_ x:A)(y:B) -> C``
  might be emitted as a function of type ``((y:B), (x:A)) -> C``.  The
  exact representation depends on the function's `calling
  convention`_, which determines the exact ordering of currying
  clauses.  Methods are treated as a form of curried function.

Layout Compatible Types
```````````````````````

(This section applies only to Swift 1.0 and will hopefully be obviated in
future releases.)

SIL tries to be ignorant of the details of type layout, and low-level
bit-banging operations such as pointer casts are generally undefined. However,
as a concession to implementation convenience, some types are allowed to be
considered **layout compatible**. Type ``T`` is *layout compatible* with type
``U`` iff:

- an address of type ``$*U`` can be cast by
  ``address_to_pointer``/``pointer_to_address`` to ``$*T`` and a valid value
  of type ``T`` can be loaded out (or indirectly used, if ``T`` is address-
  only),
- if ``T`` is a nontrivial type, then ``retain_value``/``release_value`` of
  the loaded ``T`` value is equivalent to ``retain_value``/``release_value`` of
  the original ``U`` value.

This is not always a commutative relationship; ``T`` can be layout-compatible
with ``U`` whereas ``U`` is not layout-compatible with ``T``. If the layout
compatible relationship does extend both ways, ``T`` and ``U`` are
**commutatively layout compatible**. It is however always transitive; if ``T``
is layout-compatible with ``U`` and ``U`` is layout-compatible with ``V``, then
``T`` is layout-compatible with ``V``. All types are layout-compatible with
themselves.

The following types are considered layout-compatible:

- ``Builtin.RawPointer`` is commutatively layout compatible with all heap
  object reference types, and ``Optional`` of heap object reference types.
  (Note that ``RawPointer`` is a trivial type, so does not have ownership
  semantics.)
- ``Builtin.RawPointer`` is commutatively layout compatible with
  ``Builtin.Word``.
- Structs containing a single stored property are commutatively layout
  compatible with the type of that property.
- A heap object reference is commutatively layout compatible with any type
  that can correctly reference the heap object. For instance, given a class
  ``B`` and a derived class ``D`` inheriting from ``B``, a value of
  type ``B`` referencing an instance of type ``D`` is layout compatible with
  both ``B`` and ``D``, as well as ``Builtin.NativeObject`` and
  ``Builtin.UnknownObject``. It is not layout compatible with an unrelated class
  type ``E``.
- For payloaded enums, the payload type of the first payloaded case is
  layout-compatible with the enum (*not* commutatively).

Values and Operands
~~~~~~~~~~~~~~~~~~~
::

  sil-identifier ::= [A-Za-z_0-9]+
  sil-value-name ::= '%' sil-identifier
  sil-value ::= sil-value-name
  sil-value ::= 'undef'
  sil-operand ::= sil-value ':' sil-type

SIL values are introduced with the ``%`` sigil and named by an
alphanumeric identifier, which references the instruction or basic block
argument that produces the value.  SIL values may also refer to the keyword
'undef', which is a value of undefined contents.

Unlike LLVM IR, SIL instructions that take value operands *only* accept
value operands. References to literal constants, functions, global variables, or
other entities require specialized instructions such as ``integer_literal``,
``function_ref``, ``global_addr``, etc.

Functions
~~~~~~~~~
::

  decl ::= sil-function
  sil-function ::= 'sil' sil-linkage? sil-function-name ':' sil-type
                     '{' sil-basic-block+ '}'
  sil-function-name ::= '@' [A-Za-z_0-9]+

SIL functions are defined with the ``sil`` keyword. SIL function names
are introduced with the ``@`` sigil and named by an alphanumeric
identifier. This name will become the LLVM IR name for the function,
and is usually the mangled name of the originating Swift declaration.
The ``sil`` syntax declares the function's name and SIL type, and
defines the body of the function inside braces. The declared type must
be a function type, which may be generic.

Basic Blocks
~~~~~~~~~~~~
::

  sil-basic-block ::= sil-label sil-instruction-def* sil-terminator
  sil-label ::= sil-identifier ('(' sil-argument (',' sil-argument)* ')')? ':'
  sil-argument ::= sil-value-name ':' sil-type

  sil-instruction-def ::= (sil-value-name '=')? sil-instruction
                          (',' sil-loc)? (',' sil-scope-ref)?

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
    cond_br %cond : $Builtin.Int1, then, else
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
    return %x : $Int
  }

  sil @bar : $(Int, Int) -> () {
  bb0(%x : $Int, %y : $Int):
    %foo = function_ref @foo
    %1 = apply %foo(%x) : $(Int) -> Int
    %2 = apply %foo(%y) : $(Int) -> Int
    %3 = tuple ()
    return %3 : $()
  }


Debug Information
~~~~~~~~~~~~~~~~~
::

  sil-scope-ref ::= 'scope' [0-9]+
  sil-scope ::= 'sil_scope' [0-9]+ '{'
                   sil-loc
                   'parent' scope-parent
                   ('inlined_at' sil-scope-ref)?
                '}'
  scope-parent ::= sil-function-name ':' sil-type
  scope-parent ::= sil-scope-ref
  sil-loc ::= 'loc' string-literal ':' [0-9]+ ':' [0-9]+

Each instruction may have a debug location and a SIL scope reference
at the end.  Debug locations consist of a filename, a line number, and
a column number.  If the debug location is omitted, it defaults to the
location in the SIL source file.  SIL scopes describe the position
inside the lexical scope structure that the Swift expression a SIL
instruction was generated from had originally. SIL scopes also hold
inlining information.


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
  sil-decl-subref-part ::= 'enumelt'
  sil-decl-subref-part ::= 'destroyer'
  sil-decl-subref-part ::= 'deallocator'
  sil-decl-subref-part ::= 'globalaccessor'
  sil-decl-subref-part ::= 'ivardestroyer'
  sil-decl-subref-part ::= 'ivarinitializer'
  sil-decl-subref-part ::= 'defaultarg' '.' [0-9]+
  sil-decl-uncurry-level ::= [0-9]+
  sil-decl-lang ::= 'foreign'

Some SIL instructions need to reference Swift declarations directly. These
references are introduced with the ``#`` sigil followed by the fully qualified
name of the Swift declaration. Some Swift declarations are
decomposed into multiple entities at the SIL level. These are distinguished by
following the qualified name with ``!`` and one or more ``.``-separated component
entity discriminators:

- ``getter``: the getter function for a ``var`` declaration
- ``setter``:  the setter function for a ``var`` declaration
- ``allocator``: a ``struct`` or ``enum`` constructor, or a ``class``\ 's *allocating constructor*
- ``initializer``: a ``class``\ 's *initializing constructor*
- ``enumelt``: a member of a ``enum`` type.
- ``destroyer``: a class's destroying destructor
- ``deallocator``: a class's deallocating destructor
- ``globalaccessor``: the addressor function for a global variable
- ``ivardestroyer``: a class's ivar destroyer
- ``ivarinitializer``: a class's ivar initializer
- ``defaultarg.``\ *n*: the default argument-generating function for
  the *n*\ -th argument of a Swift ``func``
- ``foreign``: a specific entry point for C/Objective-C interoperability

Methods and curried function definitions in Swift also have multiple
"uncurry levels" in SIL, representing the function at each possible
partial application level. For a curried function declaration::

  // Module example
  func foo(_ x:A)(y:B)(z:C) -> D

The declaration references and types for the different uncurry levels are as
follows::

  #example.foo!0 : $@convention(thin) (x:A) -> (y:B) -> (z:C) -> D
  #example.foo!1 : $@convention(thin) ((y:B), (x:A)) -> (z:C) -> D
  #example.foo!2 : $@convention(thin) ((z:C), (y:B), (x:A)) -> D

The deepest uncurry level is referred to as the **natural uncurry level**. In
this specific example, the reference at the natural uncurry level is
``#example.foo!2``.  Note that the uncurried argument clauses are composed
right-to-left, as specified in the `calling convention`_. For uncurry levels
less than the uncurry level, the entry point itself is ``@convention(thin)`` but
returns a thick function value carrying the partially applied arguments for its
context.

`Dynamic dispatch`_ instructions such as ``class method`` require their method
declaration reference to be uncurried to at least uncurry level 1 (which applies
both the "self" argument and the method arguments), because uncurry level zero
represents the application of the method to its "self" argument, as in
``foo.method``, which is where the dynamic dispatch semantically occurs
in Swift.

Linkage
~~~~~~~
::

  sil-linkage ::= 'public'
  sil-linkage ::= 'hidden'
  sil-linkage ::= 'shared'
  sil-linkage ::= 'private'
  sil-linkage ::= 'public_external'
  sil-linkage ::= 'hidden_external'

A linkage specifier controls the situations in which two objects in
different SIL modules are *linked*, i.e. treated as the same object.

A linkage is *external* if it ends with the suffix ``external``.  An
object must be a definition if its linkage is not external.

All functions, global variables, and witness tables have linkage.
The default linkage of a definition is ``public``.  The default linkage of a
declaration is ``public_external``.  (These may eventually change to ``hidden``
and ``hidden_external``, respectively.)

On a global variable, an external linkage is what indicates that the
variable is not a definition.  A variable lacking an explicit linkage
specifier is presumed a definition (and thus gets the default linkage
for definitions, ``public``.)

Definition of the *linked* relation
```````````````````````````````````

Two objects are linked if they have the same name and are mutually
visible:

  - An object with ``public`` or ``public_external`` linkage is always
    visible.

  - An object with ``hidden``, ``hidden_external``, or ``shared``
    linkage is visible only to objects in the same Swift module.

  - An object with ``private`` linkage is visible only to objects in
    the same SIL module.

Note that the *linked* relationship is an equivalence relation: it is
reflexive, symmetric, and transitive.

Requirements on linked objects
``````````````````````````````

If two objects are linked, they must have the same type.

If two objects are linked, they must have the same linkage, except:

  - A ``public`` object may be linked to a ``public_external`` object.

  - A ``hidden`` object may be linked to a ``hidden_external`` object.

If two objects are linked, at most one may be a definition, unless:

  - both objects have ``shared`` linkage or

  - at least one of the objects has an external linkage.

If two objects are linked, and both are definitions, then the
definitions must be semantically equivalent.  This equivalence may
exist only on the level of user-visible semantics of well-defined
code; it should not be taken to guarantee that the linked definitions
are exactly operationally equivalent.  For example, one definition of
a function might copy a value out of an address parameter, while
another may have had an analysis applied to prove that said value is
not needed.

If an object has any uses, then it must be linked to a definition
with non-external linkage.

Summary
```````

  - ``public`` definitions are unique and visible everywhere in the
    program.  In LLVM IR, they will be emitted with ``external``
    linkage and ``default`` visibility.

  - ``hidden`` definitions are unique and visible only within the
    current Swift module.  In LLVM IR, they will be emitted with
    ``external`` linkage and ``hidden`` visibility.

  - ``private`` definitions are unique and visible only within the
    current SIL module.  In LLVM IR, they will be emitted with
    ``private`` linkage.

  - ``shared`` definitions are visible only within the current Swift
    module.  They can be linked only with other ``shared``
    definitions, which must be equivalent; therefore, they only need
    to be emitted if actually used.  In LLVM IR, they will be emitted
    with ``linkonce_odr`` linkage and ``hidden`` visibility.

  - ``public_external`` and ``hidden_external`` objects always have
    visible definitions somewhere else.  If this object nonetheless
    has a definition, it's only for the benefit of optimization or
    analysis.  In LLVM IR, declarations will have ``external`` linkage
    and definitions (if actually emitted as definitions) will have
    ``available_externally`` linkage.


VTables
~~~~~~~
::

  decl ::= sil-vtable
  sil-vtable ::= 'sil_vtable' identifier '{' sil-vtable-entry* '}'

  sil-vtable-entry ::= sil-decl-ref ':' sil-linkage? sil-function-name

SIL represents dynamic dispatch for class methods using the `class_method`_,
`super_method`_, and `dynamic_method`_ instructions. The potential destinations
for these dispatch operations are tracked in ``sil_vtable`` declarations for
every class type. The declaration contains a mapping from every method of the
class (including those inherited from its base class) to the SIL function that
implements the method for that class::

  class A {
    func foo()
    func bar()
    func bas()
  }

  sil @A_foo : $@convention(thin) (@owned A) -> ()
  sil @A_bar : $@convention(thin) (@owned A) -> ()
  sil @A_bas : $@convention(thin) (@owned A) -> ()

  sil_vtable A {
    #A.foo!1: @A_foo
    #A.bar!1: @A_bar
    #A.bas!1: @A_bas
  }

  class B : A {
    func bar()
  }

  sil @B_bar : $@convention(thin) (@owned B) -> ()

  sil_vtable B {
    #A.foo!1: @A_foo
    #A.bar!1: @B_bar
    #A.bas!1: @A_bas
  }

  class C : B {
    func bas()
  }

  sil @C_bas : $@convention(thin) (@owned C) -> ()

  sil_vtable C {
    #A.foo!1: @A_foo
    #A.bar!1: @B_bar
    #A.bas!1: @C_bas
  }

Note that the declaration reference in the vtable is to the least-derived method
visible through that class (in the example above, ``B``'s vtable references
``A.bar`` and not ``B.bar``, and ``C``'s vtable references ``A.bas`` and not
``C.bas``). The Swift AST maintains override relationships between declarations
that can be used to look up overridden methods in the SIL vtable for a derived
class (such as ``C.bas`` in ``C``'s vtable).

In case the SIL function is a thunk, the function name is preceded with the
linkage of the original implementing function.

Witness Tables
~~~~~~~~~~~~~~
::

  decl ::= sil-witness-table
  sil-witness-table ::= 'sil_witness_table' sil-linkage?
                        normal-protocol-conformance '{' sil-witness-entry* '}'

SIL encodes the information needed for dynamic dispatch of generic types into
witness tables. This information is used to produce runtime dispatch tables when
generating binary code. It can also be used by SIL optimizations to specialize
generic functions. A witness table is emitted for every declared explicit
conformance. Generic types share one generic witness table for all of their
instances. Derived classes inherit the witness tables of their base class.

::

  protocol-conformance ::= normal-protocol-conformance
  protocol-conformance ::= 'inherit' '(' protocol-conformance ')'
  protocol-conformance ::= 'specialize' '<' substitution* '>'
                           '(' protocol-conformance ')'
  protocol-conformance ::= 'dependent'
  normal-protocol-conformance ::= identifier ':' identifier 'module' identifier

Witness tables are keyed by *protocol conformance*, which is a unique identifier
for a concrete type's conformance to a protocol.

- A *normal protocol conformance* names a (potentially unbound generic) type,
  the protocol it conforms to, and the module in which the type or extension
  declaration that provides the conformance appears. These correspond 1:1 to
  protocol conformance declarations in the source code.
- If a derived class conforms to a protocol through inheritance from its base
  class, this is represented by an *inherited protocol conformance*, which
  simply references the protocol conformance for the base class.
- If an instance of a generic type conforms to a protocol, it does so with a
  *specialized conformance*, which provides the generic parameter bindings
  to the normal conformance, which should be for a generic type.

Witness tables are only directly associated with normal conformances.
Inherited and specialized conformances indirectly reference the witness table of
the underlying normal conformance.

::

  sil-witness-entry ::= 'base_protocol' identifier ':' protocol-conformance
  sil-witness-entry ::= 'method' sil-decl-ref ':' sil-function-name
  sil-witness-entry ::= 'associated_type' identifier
  sil-witness-entry ::= 'associated_type_protocol'
                        '(' identifier ':' identifier ')' ':' protocol-conformance

Witness tables consist of the following entries:

- *Base protocol entries* provide references to the protocol conformances that
  satisfy the witnessed protocols' inherited protocols.
- *Method entries* map a method requirement of the protocol to a SIL function
  that implements that method for the witness type. One method entry must exist
  for every required method of the witnessed protocol.
- *Associated type entries* map an associated type requirement of the protocol
  to the type that satisfies that requirement for the witness type. Note that
  the witness type is a source-level Swift type and not a SIL type. One
  associated type entry must exist for every required associated type of the
  witnessed protocol.
- *Associated type protocol entries* map a protocol requirement on an associated
  type to the protocol conformance that satisfies that requirement for the
  associated type.

Default Witness Tables
~~~~~~~~~~~~~~~~~~~~~~
::

  decl ::= sil-default-witness-table
  sil-default-witness-table ::= 'sil_default_witness_table'
                                identifier minimum-witness-table-size
                                '{' sil-default-witness-entry* '}'
  minimum-witness-table-size ::= integer

SIL encodes requirements with resilient default implementations in a default
witness table. We say a requirement has a resilient default implementation if
the following conditions hold:

- The requirement has a default implementation
- The requirement is either the last requirement in the protocol, or all
  subsequent requirements also have resilient default implementations

The set of requirements with resilient default implementations is stored in
protocol metadata.

The minimum witness table size is the size of the witness table, in words,
not including any requirements with resilient default implementations.

Any conforming witness table must have a size between the minimum size, and
the maximum size, which is equal to the minimum size plus the number of
default requirements.

At load time, if the runtime encounters a witness table with fewer than the
maximum number of witnesses, the witness table is copied, with default
witnesses copied in. This ensures that callers can always expect to find
the correct number of requirements in each witness table, and new
requirements can be added by the framework author, without breaking client
code, as long as the new requirements have resilient default implementations.

Default witness tables are keyed by the protocol itself. Only protocols with
public visibility need a default witness table; private and internal protocols
are never seen outside the module, therefore there are no resilience issues
with adding new requirements.

::

  sil-default-witness-entry ::= 'method' sil-decl-ref ':' sil-function-name

Default witness tables currently contain only one type of entry:

- *Method entries* map a method requirement of the protocol to a SIL function
  that implements that method in a manner suitable for all witness types.

Global Variables
~~~~~~~~~~~~~~~~
::

  decl ::= sil-global-variable
  sil-global-variable ::= 'sil_global' sil-linkage identifier ':' sil-type

SIL representation of a global variable.

Global variable access is performed by the ``alloc_global`` and ``global_addr``
SIL instructions. Prior to performing any access on the global, the
``alloc_global`` instruction must be performed to initialize the storage.

Once a global's storage has been initialized, ``global_addr`` is used to
project the value.

Dataflow Errors
---------------

*Dataflow errors* may exist in raw SIL. Swift's semantics defines these
conditions as errors, so they must be diagnosed by diagnostic
passes and must not exist in canonical SIL.

Definitive Initialization
~~~~~~~~~~~~~~~~~~~~~~~~~

Swift requires that all local variables be initialized before use. In
constructors, all instance variables of a struct, enum, or class type must
be initialized before the object is used and before the constructor is returned
from.

Unreachable Control Flow
~~~~~~~~~~~~~~~~~~~~~~~~

The ``unreachable`` terminator is emitted in raw SIL to mark incorrect control
flow, such as a non-``Void`` function failing to ``return`` a value, or a
``switch`` statement failing to cover all possible values of its subject.
The guaranteed dead code elimination pass can eliminate truly unreachable
basic blocks, or ``unreachable`` instructions may be dominated by applications
of functions returning uninhabited types. An ``unreachable`` instruction that
survives guaranteed DCE and is not immediately preceded by a no-return
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

Swift Calling Convention @convention(swift)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Swift calling convention is the one used by default for native Swift
functions.

Tuples in the input type of the function are recursively destructured into
separate arguments, both in the entry point basic block of the callee, and
in the ``apply`` instructions used by callers::

  func foo(_ x:Int, y:Int)

  sil @foo : $(x:Int, y:Int) -> () {
  entry(%x : $Int, %y : $Int):
    ...
  }

  func bar(_ x:Int, y:(Int, Int))

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

  func foo(_ x:Int, y:Float) -> UnicodeScalar

  foo(x, y)

gets called in SIL as::

  %foo = constant_ref $(Int, Float) -> UnicodeScalar, @foo
  %z = apply %foo(%x, %y) : $(Int, Float) -> UnicodeScalar

Reference Counts
````````````````

*NOTE* This section only is speaking in terms of rules of thumb. The
actual behavior of arguments with respect to arguments is defined by
the argument's convention attribute (e.g. ``@owned``), not the
calling convention itself.

Reference type arguments are passed in at +1 retain count and consumed
by the callee. A reference type return value is returned at +1 and
consumed by the caller. Value types with reference type components
have their reference type components each retained and released the
same way. This Swift function::

  class A {}

  func bar(_ x:A) -> (Int, A) { ... }

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

   @API struct A {}

  func bas(_ x:A, y:Int) -> A { return x }

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

  @API struct A {}

  func zim(_ x:Int, y:A, (z:Int, w:(A, Int)))

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

  func zang(_ x:Int, (y:Int, z:Int...), v:Int, w:Int...)

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

  func curried(_ x:A)(y:B)(z:C)(w:D) -> Int {}

The types of the SIL entry points are as follows::

  sil @curried_0 : $(x:A) -> (y:B) -> (z:C) -> (w:D) -> Int { ... }
  sil @curried_1 : $((y:B), (x:A)) -> (z:C) -> (w:D) -> Int { ... }
  sil @curried_2 : $((z:C), (y:B), (x:A)) -> (w:D) -> Int { ... }
  sil @curried_3 : $((w:D), (z:C), (y:B), (x:A)) -> Int { ... }

@inout Arguments
````````````````

``@inout`` arguments are passed into the entry point by address. The callee
does not take ownership of the referenced memory. The referenced memory must
be initialized upon function entry and exit. If the ``@inout`` argument
refers to a fragile physical variable, then the argument is the address of that
variable. If the ``@inout`` argument refers to a logical property, then the
argument is the address of a caller-owned writeback buffer. It is the caller's
responsibility to initialize the buffer by storing the result of the property
getter prior to calling the function and to write back to the property
on return by loading from the buffer and invoking the setter with the final
value. This Swift function::

  func inout(_ x: inout Int) {
    x = 1
  }

gets lowered to SIL as::

  sil @inout : $(@inout Int) -> () {
  entry(%x : $*Int):
    %1 = integer_literal $Int, 1
    store %1 to %x
    return
  }

Swift Method Calling Convention @convention(method)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The method calling convention is currently identical to the freestanding
function convention. Methods are considered to be curried functions, taking
the "self" argument as their outer argument clause, and the method arguments
as the inner argument clause(s). When uncurried, the "self" argument is thus
passed last::

  struct Foo {
    func method(_ x:Int) -> Int {}
  }

  sil @Foo_method_1 : $((x : Int), @inout Foo) -> Int { ... }

Witness Method Calling Convention @convention(witness_method)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The witness method calling convention is used by protocol witness methods in
`witness tables`_. It is identical to the ``method`` calling convention
except that its handling of generic type parameters. For non-witness methods,
the machine-level convention for passing type parameter metadata may be
arbitrarily dependent on static aspects of the function signature, but because
witnesses must be polymorphically dispatchable on their ``Self`` type,
the ``Self``-related metadata for a witness must be passed in a maximally
abstracted manner.

C Calling Convention @convention(c)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Swift's C module importer, C types are always mapped to Swift types
considered trivial by SIL. SIL does not concern itself with platform
ABI requirements for indirect return, register vs. stack passing, etc.; C
function arguments and returns in SIL are always by value regardless of the
platform calling convention.

SIL (and therefore Swift) cannot currently invoke variadic C functions.

Objective-C Calling Convention @convention(objc_method)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Reference Counts
````````````````

Objective-C methods use the same argument and return value ownership rules as
ARC Objective-C. Selector families and the ``ns_consumed``,
``ns_returns_retained``, etc. attributes from imported Objective-C definitions
are honored.

Applying a ``@convention(block)`` value does not consume the block.

Method Currying
```````````````

In SIL, the "self" argument of an Objective-C method is uncurried to the last
argument of the uncurried type, just like a native Swift method::

  @objc class NSString {
    func stringByPaddingToLength(Int) withString(NSString) startingAtIndex(Int)
  }

  sil @NSString_stringByPaddingToLength_withString_startingAtIndex \
    : $((Int, NSString, Int), NSString)

That ``self`` is passed as the first argument at the IR level is abstracted
away in SIL, as is the existence of the ``_cmd`` selector argument.

Type Based Alias Analysis
-------------------------

SIL supports two types of Type Based Alias Analysis (TBAA): Class TBAA and
Typed Access TBAA.

Class TBAA
~~~~~~~~~~

Class instances and other *heap object references* are pointers at the
implementation level, but unlike SIL addresses, they are first class values and
can be ``capture``-d and aliased. Swift, however, is memory-safe and statically
typed, so aliasing of classes is constrained by the type system as follows:

* A ``Builtin.NativeObject`` may alias any native Swift heap object,
  including a Swift class instance, a box allocated by ``alloc_box``,
  or a thick function's closure context.
  It may not alias natively Objective-C class instances.
* A ``Builtin.UnknownObject`` or ``Builtin.BridgeObject`` may alias
  any class instance, whether Swift or Objective-C, but may not alias
  non-class-instance heap objects.
* Two values of the same class type ``$C`` may alias. Two values of related
  class type ``$B`` and ``$D``, where there is a subclass relationship between
  ``$B`` and ``$D``, may alias. Two values of unrelated class types may not
  alias. This includes different instantiations of a generic class type, such
  as ``$C<Int>`` and ``$C<Float>``, which currently may never alias.
* Without whole-program visibility, values of archetype or protocol type must
  be assumed to potentially alias any class instance. Even if it is locally
  apparent that a class does not conform to that protocol, another component
  may introduce a conformance by an extension. Similarly, a generic class
  instance, such as ``$C<T>`` for archetype ``T``, must be assumed to
  potentially alias concrete instances of the generic type, such as
  ``$C<Int>``, because ``Int`` is a potential substitution for ``T``.

A violation of the above aliasing rules only results in undefined
behavior if the aliasing references are dereferenced within Swift code.
For example,
``_SwiftNativeNS[Array|Dictionary|String]`` classes alias with
``NS[Array|Dictionary|String]`` classes even though they are not
statically related. Since Swift never directly accesses stored
properties on the Foundation classes, this aliasing does not pose a
danger.

Typed Access TBAA
~~~~~~~~~~~~~~~~~

Define a *typed access* of an address or reference as one of the following:

* Any instruction that performs a typed read or write operation upon the memory
  at the given location (e.x. ``load``, ``store``).
* Any instruction that yields a typed offset of the pointer by performing a
  typed projection operation (e.x. ``ref_element_addr``,
  ``tuple_element_addr``).

With limited exceptions, it is undefined behavior to perform a typed access to
an address or reference addressed memory is not bound to the relevant type.

This allows the optimizer to assume that two addresses cannot alias if
there does not exist a substitution of archetypes that could cause one
of the types to be the type of a subobject of the other. Additionally,
this applies to the types of the values from which the addresses were
derived via a typed projection.

Consider the following SIL::

  struct Element {
    var i: Int
  }
  struct S1 {
    var elt: Element
  }
  struct S2 {
    var elt: Element
  }
  %adr1 = struct_element_addr %ptr1 : $*S1, #S.elt
  %adr2 = struct_element_addr %ptr2 : $*S2, #S.elt

The optimizer may assume that ``%adr1`` does not alias with ``%adr2``
because the values that the addresses are derived from (``%ptr1`` and
``%ptr2``) have unrelated types. However, in the following example,
the optimizer cannot assume that ``%adr1`` does not alias with
``%adr2`` because ``%adr2`` is derived from a cast, and any subsequent
typed operations on the address will refer to the common ``Element`` type::

  %adr1 = struct_element_addr %ptr1 : $*S1, #S.elt
  %adr2 = pointer_to_address %ptr2 : $Builtin.RawPointer to $*Element

Exceptions to typed access TBAA rules are only allowed for blessed
alias-introducing operations. This permits limited type-punning. The only
current exception is the non-struct ``pointer_to_address`` variant. The
optimizer must be able to defensively determine that none of the *roots* of an
address are alias-introducing operations. An address root is the operation that
produces the address prior to applying any typed projections, indexing, or
casts. The following are valid address roots:

* Object allocation that generates an address, such as ``alloc_stack``
  and ``alloc_box``.

* Address-type function arguments. These are crucially *not* considered
  alias-introducing operations. It is illegal for the SIL optimizer to
  form a new function argument from an arbitrary address-type
  value. Doing so would require the optimizer to guarantee that the
  new argument is both has a non-alias-introducing address root and
  can be properly represented by the calling convention (address types
  do not have a fixed representation).

* A strict cast from an untyped pointer, ``pointer_to_address [strict]``. It is
  illegal for ``pointer_to_address [strict]`` to derive its address from an
  alias-introducing operation's value. A type punned address may only be
  produced from an opaque pointer via a non-strict ``pointer_to_address`` at the
  point of conversion.

Address-to-address casts, via ``unchecked_addr_cast``, transparently
forward their source's address root, just like typed projections.

Address-type basic block arguments can be conservatively considered
aliasing-introducing operations; they are uncommon enough not to
matter and may eventually be prohibited altogether.

Although some pointer producing intrinsics exist, they do not need to be
considered alias-introducing exceptions to TBAA rules. ``Builtin.inttoptr``
produces a ``Builtin.RawPointer`` which is not interesting because by definition
it may alias with everything. Similarly, the LLVM builtins ``Builtin.bitcast``
and ``Builtin.trunc|sext|zextBitCast`` cannot produce typed pointers. These
pointer values must be converted to an address via ``pointer_to_address`` before
typed access can occur. Whether the ``pointer_to_address`` is strict determines
whether aliasing may occur.

Memory may be rebound to an unrelated type. Addresses to unrelated types may
alias as long as typed access only occurs while memory is bound to the relevant
type. Consequently, the optimizer cannot outright assume that addresses accessed
as unrelated types are nonaliasing. For example, pointer comparison cannot be
eliminated simply because the two addresses derived from those pointers are
accessed as unrelated types at different program points.

Value Dependence
----------------

In general, analyses can assume that independent values are
independently assured of validity.  For example, a class method may
return a class reference::

  bb0(%0 : $MyClass):
    %1 = class_method %0 : $MyClass, #MyClass.foo!1
    %2 = apply %1(%0) : $@convention(method) (@guaranteed MyClass) -> @owned MyOtherClass
    // use of %2 goes here; no use of %1
    strong_release %2 : $MyOtherClass
    strong_release %1 : $MyClass

The optimizer is free to move the release of ``%1`` to immediately
after the call here, because ``%2`` can be assumed to be an
independently-managed value, and because Swift generally permits the
reordering of destructors.

However, some instructions do create values that are intrinsically
dependent on their operands.  For example, the result of
``ref_element_addr`` will become a dangling pointer if the base is
released too soon.  This is captured by the concept of *value dependence*,
and any transformation which can reorder of destruction of a value
around another operation must remain conscious of it.

A value ``%1`` is said to be *value-dependent* on a value ``%0`` if:

- ``%1`` is the result and ``%0`` is the first operand of one of the
  following instructions:

  - ``ref_element_addr``
  - ``struct_element_addr``
  - ``tuple_element_addr``
  - ``unchecked_take_enum_data_addr``
  - ``pointer_to_address``
  - ``address_to_pointer``
  - ``index_addr``
  - ``index_raw_pointer``
  - possibly some other conversions

- ``%1`` is the result of ``mark_dependence`` and ``%0`` is either of
  the operands.

- ``%1`` is the value address of a box allocation instruction of which
  ``%0`` is the box reference.

- ``%1`` is the result of a ``struct``, ``tuple``, or ``enum``
  instruction and ``%0`` is an operand.

- ``%1`` is the result of projecting out a subobject of ``%0``
  with ``tuple_extract``, ``struct_extract``, ``unchecked_enum_data``,
  ``select_enum``, or ``select_enum_addr``.

- ``%1`` is the result of ``select_value`` and ``%0`` is one of the cases.

- ``%1`` is a basic block parameter and ``%0`` is the corresponding
  argument from a branch to that block.

- ``%1`` is the result of a ``load`` from ``%0``.  However, the value
  dependence is cut after the first attempt to manage the value of
  ``%1``, e.g. by retaining it.

- Transitivity: there exists a value ``%2`` which ``%1`` depends on
  and which depends on ``%0``.  However, transitivity does not apply
  to different subobjects of a struct, tuple, or enum.

Note, however, that an analysis is not required to track dependence
through memory.  Nor is it required to consider the possibility of
dependence being established "behind the scenes" by opaque code, such
as by a method returning an unsafe pointer to a class property.  The
dependence is required to be locally obvious in a function's SIL
instructions.  Precautions must be taken against this either by SIL
generators (by using ``mark_dependence`` appropriately) or by the user
(by using the appropriate intrinsics and attributes with unsafe
language or library features).

Only certain types of SIL value can carry value-dependence:

- SIL address types
- unmanaged pointer types:

  - ``@sil_unmanaged`` types
  - ``Builtin.RawPointer``
  - aggregates containing such a type, such as ``UnsafePointer``,
    possibly recursively

- non-trivial types (but they can be independently managed)

This rule means that casting a pointer to an integer type breaks
value-dependence.  This restriction is necessary so that reading an
``Int`` from a class doesn't force the class to be kept around!
A class holding an unsafe reference to an object must use some
sort of unmanaged pointer type to do so.

This rule does not include generic or resilient value types which
might contain unmanaged pointer types.  Analyses are free to assume
that e.g. a ``copy_addr`` of a generic or resilient value type yields
an independently-managed value.  The extension of value dependence to
types containing obvious unmanaged pointer types is an affordance to
make the use of such types more convenient; it does not shift the
ultimate responsibility for assuring the safety of unsafe
language/library features away from the user.

Instruction Set
---------------

Allocation and Deallocation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

These instructions allocate and deallocate memory.

alloc_stack
```````````
::

  sil-instruction ::= 'alloc_stack' sil-type (',' debug-var-attr)*

  %1 = alloc_stack $T
  // %1 has type $*T

Allocates uninitialized memory that is sufficiently aligned on the stack
to contain a value of type ``T``. The result of the instruction is the address
of the allocated memory.

If a type is runtime-sized, the compiler must emit code to potentially
dynamically allocate memory. So there is no guarantee that the allocated
memory is really located on the stack.

``alloc_stack`` marks the start of the lifetime of the value; the
allocation must be balanced with a ``dealloc_stack`` instruction to
mark the end of its lifetime. All ``alloc_stack`` allocations must be
deallocated prior to returning from a function. If a block has multiple
predecessors, the stack height and order of allocations must be consistent
coming from all predecessor blocks. ``alloc_stack`` allocations must be
deallocated in last-in, first-out stack order.

The memory is not retainable. To allocate a retainable box for a value
type, use ``alloc_box``.

alloc_ref
`````````
::

  sil-instruction ::= 'alloc_ref'
                        ('[' 'objc' ']')?
                        ('[' 'stack' ']')?
                        ('[' 'tail_elems' sil-type '*' sil-operand ']')*
                        sil-type

  %1 = alloc_ref [stack] $T
  %1 = alloc_ref [tail_elems $E * %2 : Builtin.Word] $T
  // $T must be a reference type
  // %1 has type $T
  // $E is the type of the tail-allocated elements
  // %2 must be of a builtin integer type

Allocates an object of reference type ``T``. The object will be initialized
with retain count 1; its state will be otherwise uninitialized. The
optional ``objc`` attribute indicates that the object should be
allocated using Objective-C's allocation methods (``+allocWithZone:``).

The optional ``stack`` attribute indicates that the object can be allocated
on the stack instead on the heap. In this case the instruction must have
balanced with a ``dealloc_ref [stack]`` instruction to mark the end of the
object's lifetime.
Note that the ``stack`` attribute only specifies that stack allocation is
possible. The final decision on stack allocation is done during llvm IR
generation. This is because the decision also depends on the object size,
which is not necessarily known at SIL level.

The optional ``tail_elems`` attributes specifies the amount of space to be
reserved for tail-allocated arrays of given element types and element counts.
If there are more than one ``tail_elems`` attributes then the tail arrays are
allocated in the specified order.
The count-operand must be of a builtin integer type.
The instructions ``ref_tail_addr`` and ``tail_addr`` can be used to project
the tail elements.
The ``objc`` attribute cannot be used together with ``tail_elems``.

alloc_ref_dynamic
`````````````````
::

  sil-instruction ::= 'alloc_ref_dynamic'
                        ('[' 'objc' ']')?
                        ('[' 'tail_elems' sil-type '*' sil-operand ']')*
                        sil-operand ',' sil-type

  %1 = alloc_ref_dynamic %0 : $@thick T.Type, $T
  %1 = alloc_ref_dynamic [objc] %0 : $@objc_metatype T.Type, $T
  %1 = alloc_ref_dynamic [tail_elems $E * %2 : Builtin.Word] %0 : $@thick T.Type, $T
  // $T must be a class type
  // %1 has type $T
  // $E is the type of the tail-allocated elements
  // %2 must be of a builtin integer type

Allocates an object of class type ``T`` or a subclass thereof. The
dynamic type of the resulting object is specified via the metatype
value ``%0``. The object will be initialized with retain count 1; its
state will be otherwise uninitialized.

The optional ``tail_elems`` and ``objc`` attributes have the same effect as
for ``alloc_ref``. See ``alloc_ref`` for details.

alloc_box
`````````
::

  sil-instruction ::= 'alloc_box' sil-type (',' debug-var-attr)*

  %1 = alloc_box $T
  //   %1 has type $@box T

Allocates a reference-counted ``@box`` on the heap large enough to hold a value
of type ``T``, along with a retain count and any other metadata required by the
runtime.  The result of the instruction is the reference-counted ``@box``
reference that owns the box. The ``project_box`` instruction is used to retrieve
the address of the value inside the box.

The box will be initialized with a retain count of 1; the storage will be
uninitialized. The box owns the contained value, and releasing it to a retain
count of zero destroys the contained value as if by ``destroy_addr``.
Releasing a box is undefined behavior if the box's value is uninitialized.
To deallocate a box whose value has not been initialized, ``dealloc_box``
should be used.

alloc_value_buffer
``````````````````

::

   sil-instruction ::= 'alloc_value_buffer' sil-type 'in' sil-operand

   %1 = alloc_value_buffer $(Int, T) in %0 : $*Builtin.UnsafeValueBuffer
   // The operand must have the exact type shown.
   // The result has type $*(Int, T).

Given the address of an unallocated value buffer, allocate space in it
for a value of the given type.  This instruction has undefined
behavior if the value buffer is currently allocated.

The type operand must be a lowered object type.

alloc_global
````````````

::

   sil-instruction ::= 'alloc_global' sil-global-name

   alloc_global @foo

Initialize the storage for a global variable. This instruction has
undefined behavior if the global variable has already been initialized.

The type operand must be a lowered object type.

dealloc_stack
`````````````
::

  sil-instruction ::= 'dealloc_stack' sil-operand

  dealloc_stack %0 : $*T
  // %0 must be of $*T type

Deallocates memory previously allocated by ``alloc_stack``. The
allocated value in memory must be uninitialized or destroyed prior to
being deallocated. This instruction marks the end of the lifetime for
the value created by the corresponding ``alloc_stack`` instruction. The operand
must be the shallowest live ``alloc_stack`` allocation preceding the
deallocation. In other words, deallocations must be in last-in, first-out
stack order.

dealloc_box
```````````
::

  sil-instruction ::= 'dealloc_box' sil-operand

  dealloc_box %0 : $@box T

Deallocates a box, bypassing the reference counting mechanism. The box
variable must have a retain count of one. The boxed type must match the
type passed to the corresponding ``alloc_box`` exactly, or else
undefined behavior results.

This does not destroy the boxed value. The contents of the
value must have been fully uninitialized or destroyed before
``dealloc_box`` is applied.

project_box
```````````
::

  sil-instruction ::= 'project_box' sil-operand

  %1 = project_box %0 : $@box T

  // %1 has type $*T

Given a ``@box T`` reference, produces the address of the value inside the box.

dealloc_ref
```````````
::

  sil-instruction ::= 'dealloc_ref' ('[' 'stack' ']')? sil-operand

  dealloc_ref [stack] %0 : $T
  // $T must be a class type

Deallocates an uninitialized class type instance, bypassing the reference
counting mechanism.

The type of the operand must match the allocated type exactly, or else
undefined behavior results.

The instance must have a retain count of one.

This does not destroy stored properties of the instance. The contents
of stored properties must be fully uninitialized at the time
``dealloc_ref`` is applied.

The ``stack`` attribute indicates that the instruction is the balanced
deallocation of its operand which must be a ``alloc_ref [stack]``.
In this case the instruction marks the end of the object's lifetime but
has no other effect.

dealloc_partial_ref
```````````````````
::

  sil-instruction ::= 'dealloc_partial_ref' sil-operand sil-metatype

  dealloc_partial_ref %0 : $T, %1 : $U.Type
  // $T must be a class type
  // $T must be a subclass of U

Deallocates a partially-initialized class type instance, bypassing
the reference counting mechanism.

The type of the operand must be a supertype of the allocated type, or
else undefined behavior results.

The instance must have a retain count of one.

All stored properties in classes more derived than the given metatype
value must be initialized, and all other stored properties must be
uninitialized. The initialized stored properties are destroyed before
deallocating the memory for the instance.

This does not destroy the reference type instance. The contents of the
heap object must have been fully uninitialized or destroyed before
``dealloc_ref`` is applied.

dealloc_value_buffer
````````````````````

::

   sil-instruction ::= 'dealloc_value_buffer' sil-type 'in' sil-operand

   dealloc_value_buffer $(Int, T) in %0 : $*Builtin.UnsafeValueBuffer
   // The operand must have the exact type shown.

Given the address of a value buffer, deallocate the storage in it.
This instruction has undefined behavior if the value buffer is not
currently allocated, or if it was allocated with a type other than the
type operand.

The type operand must be a lowered object type.

project_value_buffer
````````````````````

::

   sil-instruction ::= 'project_value_buffer' sil-type 'in' sil-operand

   %1 = project_value_buffer $(Int, T) in %0 : $*Builtin.UnsafeValueBuffer
   // The operand must have the exact type shown.
   // The result has type $*(Int, T).

Given the address of a value buffer, return the address of the value
storage in it.  This instruction has undefined behavior if the value
buffer is not currently allocated, or if it was allocated with a type
other than the type operand.

The result is the same value as was originally returned by
``alloc_value_buffer``.

The type operand must be a lowered object type.

Debug Information
~~~~~~~~~~~~~~~~~

Debug information is generally associated with allocations (alloc_stack or
alloc_box) by having a Decl node attached to the allocation with a SILLocation.
For declarations that have no allocation we have explicit instructions for
doing this.  This is used by 'let' declarations, which bind a value to a name
and for var decls who are promoted into registers.  The decl they refer to is
attached to the instruction with a SILLocation.

debug_value
```````````

::

  sil-instruction ::= debug_value sil-operand (',' debug-var-attr)*

  debug_value %1 : $Int

This indicates that the value of a declaration with loadable type has changed
value to the specified operand.  The declaration in question is identified by
the SILLocation attached to the debug_value instruction.

The operand must have loadable type.

::

   debug-var-attr ::= 'var'
   debug-var-attr ::= 'let'
   debug-var-attr ::= 'name' string-literal
   debug-var-attr ::= 'argno' integer-literal

There are a number of attributes that provide details about the source
variable that is being described, including the name of the
variable. For function and closure arguments ``argno`` is the number
of the function argument starting with 1.

debug_value_addr
````````````````

::

  sil-instruction ::= debug_value_addr sil-operand (',' debug-var-attr)*

  debug_value_addr %7 : $*SomeProtocol

This indicates that the value of a declaration with address-only type
has changed value to the specified operand.  The declaration in
question is identified by the SILLocation attached to the
debug_value_addr instruction.


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
load from uninitialized memory or to load from an address that points to
deallocated storage.

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
stored over and cleaned up. It is undefined behavior to store to an address
that points to deallocated storage.

load_borrow
```````````

::

   sil-instruction ::= 'load_borrow' sil-value

   %1 = load_borrow %0 : $*T
   // $T must be a loadable type

Loads the value ``%1`` from the memory location ``%0``. The ``load_borrow``
instruction creates a borrowed scope in which a read-only borrow value ``%1``
can be used to read the value stored in ``%0``. The end of scope is delimited
by an ``end_borrow`` instruction. All ``load_borrow`` instructions must be
paired with exactly one ``end_borrow`` instruction along any path through the
program. Until ``end_borrow``, it is illegal to invalidate or store to ``%0``.

end_borrow
``````````

::

   sil-instruction ::= 'end_borrow' sil-value 'from' sil-value : sil-type, sil-type

   end_borrow %1 from %0 : $T, $T
   end_borrow %1 from %0 : $T, $*T
   end_borrow %1 from %0 : $*T, $T
   end_borrow %1 from %0 : $*T, $*T
   // We allow for end_borrow to be specified in between values and addresses
   // all of the same type T.

Ends the scope for which the SILValue ``%1`` is borrowed from the SILValue
``%0``. Must be paired with at most 1 borrowing instruction (like
``load_borrow``) along any path through the program. In the region in between
the borrow instruction and the ``end_borrow``, the original SILValue can not be
modified. This means that:

1. If ``%0`` is an address, ``%0`` can not be written to.
2. If ``%0`` is a non-trivial value, ``%0`` can not be destroyed.

We require that ``%1`` and ``%0`` have the same type ignoring SILValueCategory.

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

mark_uninitialized
``````````````````
::

  sil-instruction ::= 'mark_uninitialized' '[' mu_kind ']' sil-operand
  mu_kind ::= 'var'
  mu_kind ::= 'rootself'
  mu_kind ::= 'derivedself'
  mu_kind ::= 'derivedselfonly'
  mu_kind ::= 'delegatingself'

  %2 = mark_uninitialized [var] %1 : $*T
  // $T must be an address

Indicates that a symbolic memory location is uninitialized, and must be
explicitly initialized before it escapes or before the current function returns.
This instruction returns its operands, and all accesses within the function must
be performed against the return value of the mark_uninitialized instruction.

The kind of mark_uninitialized instruction specifies the type of data
the mark_uninitialized instruction refers to:

- ``var``: designates the start of a normal variable live range
- ``rootself``: designates ``self`` in a struct, enum, or root class
- ``derivedself``: designates ``self`` in a derived (non-root) class
- ``derivedselfonly``: designates ``self`` in a derived (non-root) class whose stored properties have already been initialized
- ``delegatingself``: designates ``self`` on a struct, enum, or class in a delegating constructor (one that calls self.init)

The purpose of the ``mark_uninitialized`` instruction is to enable
definitive initialization analysis for global variables (when marked as
'globalvar') and instance variables (when marked as 'rootinit'), which need to
be distinguished from simple allocations.

It is produced by SILGen, and is only valid in Raw SIL.  It is rewritten as
appropriate by the definitive initialization pass.

mark_function_escape
````````````````````
::

  sil-instruction ::= 'mark_function_escape' sil-operand (',' sil-operand)

  %2 = mark_function_escape %1 : $*T

Indicates that a function definition closes over a symbolic memory location.
This instruction is variadic, and all of its operands must be addresses.

The purpose of the ``mark_function_escape`` instruction is to enable
definitive initialization analysis for global variables and instance variables,
which are not represented as box allocations.

It is produced by SILGen, and is only valid in Raw SIL.  It is rewritten as
appropriate by the definitive initialization pass.

mark_uninitialized_behavior
```````````````````````````
::

   init-case ::= sil-value sil-apply-substitution-list? '(' sil-value ')' ':' sil-type
   set-case ::= sil-value sil-apply-substitution-list? '(' sil-value ')' ':' sil-type
   sil-instruction ::= 'mark_uninitialized_behavior' init-case set-case

   mark_uninitialized_behavior %init<Subs>(%storage) : $T -> U,
                               %set<Subs>(%self) : $V -> W

Indicates that a logical property is uninitialized at this point and needs to be
initialized by the end of the function and before any escape point for this
instruction. Assignments to the property trigger the behavior's ``init`` or
``set`` logic based on the logical initialization state of the property.

It is expected that the ``init-case`` is passed some sort of storage and the
``set`` case is passed ``self``.

This is only valid in Raw SIL.

copy_addr
`````````
::

  sil-instruction ::= 'copy_addr' '[take]'? sil-value
                        'to' '[initialization]'? sil-operand

  copy_addr [take] %0 to [initialization] %1 : $*T
  // %0 and %1 must be of the same $*T address type

Loads the value at address ``%0`` from memory and assigns a copy of it back into
memory at address ``%1``. A bare ``copy_addr`` instruction when ``T`` is a
non-trivial type::

  copy_addr %0 to %1 : $*T

is equivalent to::

  %new = load %0 : $*T        // Load the new value from the source
  %old = load %1 : $*T        // Load the old value from the destination
  strong_retain %new : $T            // Retain the new value
  strong_release %old : $T           // Release the old
  store %new to %1 : $*T      // Store the new value to the destination

except that ``copy_addr`` may be used even if ``%0`` is of an address-only
type. The ``copy_addr`` may be given one or both of the ``[take]`` or
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

If ``T`` is a trivial type, then ``copy_addr`` is always equivalent to its
take-initialization form.

destroy_addr
````````````
::

  sil-instruction ::= 'destroy_addr' sil-operand

  destroy_addr %0 : $*T
  // %0 must be of an address $*T type

Destroys the value in memory at address ``%0``. If ``T`` is a non-trivial type,
This is equivalent to::

  %1 = load %0
  strong_release %1

except that ``destroy_addr`` may be used even if ``%0`` is of an
address-only type.  This does not deallocate memory; it only destroys the
pointed-to value, leaving the memory uninitialized.

If ``T`` is a trivial type, then ``destroy_addr`` is a no-op.

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
a contiguous array. It is undefined to try to reference offsets within a
non-array value, such as fields within a homogeneous struct or tuple type, or
bytes within a value, using ``index_addr``. (``Int8`` address types have no
special behavior in this regard, unlike ``char*`` or ``void*`` in C.) It is
also undefined behavior to index out of bounds of an array, except to index
the "past-the-end" address of the array.

tail_addr
`````````
::

  sil-instruction ::= 'tail_addr' sil-operand ',' sil-operand ',' sil-type

  %2 = tail_addr %0 : $*T, %1 : $Builtin.Int<n>, $E
  // %0 must be of an address type $*T
  // %1 must be of a builtin integer type
  // %2 will be of type $*E

Given an address of an array of ``%1`` values, returns the address of an
element which is tail-allocated after the array.
This instruction is equivalent to ``index_addr`` except that the resulting
address is aligned-up to the tail-element type ``$E``.

This instruction is used to project the N-th tail-allocated array from an
object which is created by an ``alloc_ref`` with multiple ``tail_elems``.
The first operand is the address of an element of the (N-1)-th array, usually
the first element. The second operand is the number of elements until the end
of that array. The result is the address of the first element of the N-th array.

It is undefined behavior if the provided address, count and type do not match
the actual layout of tail-allocated arrays of the underlying object.

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

bind_memory
```````````

::

  sil-instruction ::= 'bind_memory' sil-operand ',' sil-operand 'to' sil-type

  bind_memory %0 : $Builtin.RawPointer, %1 : $Builtin.Word to $T
  // %0 must be of $Builtin.RawPointer type
  // %1 must be of $Builtin.Word type

Binds memory at ``Builtin.RawPointer`` value ``%0`` to type ``$T`` with enough
capacity to hold ``%1`` values. See SE-0107: UnsafeRawPointer.

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
   uses a ``@weak`` or ``@unowned`` reference as if it were a strong
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

strong_release
``````````````
::

  strong_release %0 : $T
  // $T must be a reference type.

Decrements the strong reference count of the heap object referenced by ``%0``.
If the release operation brings the strong reference count of the object to
zero, the object is destroyed and ``@weak`` references are cleared.  When both
its strong and unowned reference counts reach zero, the object's memory is
deallocated.

set_deallocating
````````````````
::

  set_deallocating %0 : $T
  // $T must be a reference type.

Explicitly sets the state of the object referenced by ``%0`` to deallocated.
This is the same operation what's done by a strong_release immediately before
it calls the deallocator of the object.

It is expected that the strong reference count of the object is one.
Furthermore, no other thread may increment the strong reference count during
execution of this instruction.

strong_retain_unowned
`````````````````````
::

  sil-instruction ::= 'strong_retain_unowned' sil-operand

  strong_retain_unowned %0 : $@unowned T
  // $T must be a reference type

Asserts that the strong reference count of the heap object referenced by ``%0``
is still positive, then increases it by one.

unowned_retain
``````````````
::

  sil-instruction ::= 'unowned_retain' sil-operand

  unowned_retain %0 : $@unowned T
  // $T must be a reference type

Increments the unowned reference count of the heap object underlying ``%0``.

unowned_release
```````````````
::

  sil-instruction ::= 'unowned_release' sil-operand

  unowned_release %0 : $@unowned T
  // $T must be a reference type

Decrements the unowned reference count of the heap object referenced by
``%0``.  When both its strong and unowned reference counts reach zero,
the object's memory is deallocated.

load_weak
`````````

::

  sil-instruction ::= 'load_weak' '[take]'? sil-operand

  load_weak [take] %0 : $*@sil_weak Optional<T>
  // $T must be an optional wrapping a reference type

Increments the strong reference count of the heap object held in the operand,
which must be an initialized weak reference.  The result is value of type
``$Optional<T>``, except that it is ``null`` if the heap object has begun
deallocation.

This operation must be atomic with respect to the final ``strong_release`` on
the operand heap object.  It need not be atomic with respect to ``store_weak``
operations on the same address.

store_weak
``````````

::

  sil-instruction ::= 'store_weak' sil-value 'to' '[initialization]'? sil-operand

  store_weak %0 to [initialization] %1 : $*@sil_weak Optional<T>
  // $T must be an optional wrapping a reference type

Initializes or reassigns a weak reference.  The operand may be ``nil``.

If ``[initialization]`` is given, the weak reference must currently either be
uninitialized or destroyed.  If it is not given, the weak reference must
currently be initialized.

This operation must be atomic with respect to the final ``strong_release`` on
the operand (source) heap object.  It need not be atomic with respect to
``store_weak`` or ``load_weak`` operations on the same address.

load_unowned
````````````

TODO: Fill this in

store_unowned
`````````````

TODO: Fill this in

fix_lifetime
````````````

::

  sil-instruction :: 'fix_lifetime' sil-operand

  fix_lifetime %0 : $T
  // Fix the lifetime of a value %0
  fix_lifetime %1 : $*T
  // Fix the lifetime of the memory object referenced by %1

Acts as a use of a value operand, or of the value in memory referenced by an
address operand. Optimizations may not move operations that would destroy the
value, such as ``release_value``, ``strong_release``, ``copy_addr [take]``, or
``destroy_addr``, past this instruction.

mark_dependence
```````````````

::

  sil-instruction :: 'mark_dependence' sil-operand 'on' sil-operand

  %2 = mark_dependence %0 : $*T on %1 : $Builtin.NativeObject

Indicates that the validity of the first operand depends on the value
of the second operand.  Operations that would destroy the second value
must not be moved before any instructions which depend on the result
of this instruction, exactly as if the address had been obviously
derived from that operand (e.g. using ``ref_element_addr``).

The result is always equal to the first operand.  The first operand
will typically be an address, but it could be an address in a
non-obvious form, such as a Builtin.RawPointer or a struct containing
the same.  Transformations should be somewhat forgiving here.

The second operand may have either object or address type.  In the
latter case, the dependency is on the current value stored in the
address.

strong_pin
``````````

TODO: Fill me in!

strong_unpin
````````````

TODO: Fill me in!

is_unique
`````````

::

  sil-instruction ::= 'is_unique' sil-operand

  %1 = is_unique %0 : $*T
  // $T must be a reference-counted type
  // %1 will be of type Builtin.Int1

Checks whether %0 is the address of a unique reference to a memory
object. Returns 1 if the strong reference count is 1, and 0 if the
strong reference count is greater than 1.

A discussion of the semantics can be found here:
:ref:`arcopts.is_unique`.

is_unique_or_pinned
```````````````````

::

  sil-instruction ::= 'is_unique_or_pinned' sil-operand

  %1 = is_unique_or_pinned %0 : $*T
  // $T must be a reference-counted type
  // %1 will be of type Builtin.Int1

Checks whether %0 is the address of either a unique reference to a
memory object or a reference to a pinned object. Returns 1 if the
strong reference count is 1 or the object has been marked pinned by
strong_pin.

copy_block
``````````
::

  sil-instruction :: 'copy_block' sil-operand

  %1 = copy_block %0 : $@convention(block) T -> U

Performs a copy of an Objective-C block. Unlike retains of other
reference-counted types, this can produce a different value from the operand
if the block is copied from the stack to the heap.

builtin "unsafeGuaranteed"
``````````````````````````

::

  sil-instruction := 'builtin' '"unsafeGuaranteed"' '<' sil-type '>' '(' sil-operand')' ':' sil-type

  %1 = builtin "unsafeGuaranteed"<T>(%0 : $T) : ($T, Builtin.Int1)
  // $T must be of AnyObject type.

Asserts that there exists another reference of the value ``%0`` for the scope
delineated by the call of this builtin up to the first call of a ``builtin
"unsafeGuaranteedEnd"`` instruction that uses the second element ``%1.1`` of the
returned value. If no such instruction can be found nothing can be assumed. This
assertions holds for uses of the first tuple element of the returned value
``%1.0`` within this scope. The returned reference value equals the input
``%0``.

builtin "unsafeGuaranteedEnd"
`````````````````````````````

::

  sil-instruction := 'builtin' '"unsafeGuaranteedEnd"' '(' sil-operand')'

  %1 = builtin "unsafeGuaranteedEnd"(%0 : $Builtin.Int1)
  // $T must be of AnyObject type.

Ends the scope for the ``builtin "unsafeGuaranteed"`` instruction.

Literals
~~~~~~~~

These instructions bind SIL values to literal constants or to global entities.

function_ref
````````````
::

  sil-instruction ::= 'function_ref' sil-function-name ':' sil-type

  %1 = function_ref @function : $@convention(thin) T -> U
  // $@convention(thin) T -> U must be a thin function type
  // %1 has type $T -> U

Creates a reference to a SIL function.

global_addr
```````````

::

  sil-instruction ::= 'global_addr' sil-global-name ':' sil-type

  %1 = global_addr @foo : $*Builtin.Word

Creates a reference to the address of a global variable which has been
previously initialized by ``alloc_global``. It is undefined behavior to
perform this operation on a global variable which has not been
initialized.

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

  sil-instruction ::= 'string_literal' encoding string-literal
  encoding ::= 'utf8'
  encoding ::= 'utf16'
  encoding ::= 'objc_selector'

  %1 = string_literal "asdf"
  // %1 has type $Builtin.RawPointer

Creates a reference to a string in the global string table. The result
is a pointer to the data.  The referenced string is always null-terminated. The
string literal value is specified using Swift's string
literal syntax (though ``\()`` interpolations are not allowed). When
the encoding is ``objc_selector``, the string literal produces a
reference to a UTF-8-encoded Objective-C selector in the Objective-C
method name segment.

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
(indicated by the ``foreign`` marker on a method reference, as in
``#NSObject.description!1.foreign``), then the instruction
represents an ``objc_msgSend`` invocation. ``objc_msgSend`` invocations can
only be used as the callee of an ``apply`` instruction or ``partial_apply``
instruction. They cannot be stored or used as ``apply`` or ``partial_apply``
arguments.  ``objc_msgSend`` invocations must always be ``volatile``.

class_method
````````````
::

  sil-instruction ::= 'class_method' sil-method-attributes?
                        sil-operand ',' sil-decl-ref ':' sil-type

  %1 = class_method %0 : $T, #T.method!1 : $@convention(thin) U -> V
  // %0 must be of a class type or class metatype $T
  // #T.method!1 must be a reference to a dynamically-dispatched method of T or
  // of one of its superclasses, at uncurry level >= 1
  // %1 will be of type $U -> V

Looks up a method based on the dynamic type of a class or class metatype
instance. It is undefined behavior if the class value is null and the
method is not an Objective-C method.

If:

- the instruction is not ``[volatile]``,
- the referenced method is not a ``foreign`` method,
- and the static type of the class instance is known, or the method is known
  to be final,

then the instruction is a candidate for devirtualization optimization. A
devirtualization pass can consult the module's `VTables`_ to find the
SIL function that implements the method and promote the instruction to a
static `function_ref`_.

super_method
````````````
::

  sil-instruction ::= 'super_method' sil-method-attributes?
                        sil-operand ',' sil-decl-ref ':' sil-type

  %1 = super_method %0 : $T, #Super.method!1.foreign : $@convention(thin) U -> V
  // %0 must be of a non-root class type or class metatype $T
  // #Super.method!1.foreign must be a reference to an ObjC method of T's
  // superclass or of one of its ancestor classes, at uncurry level >= 1
  // %1 will be of type $@convention(thin) U -> V

Looks up a method in the superclass of a class or class metatype instance.
Note that for native Swift methods, ``super.method`` calls are statically
dispatched, so this instruction is only valid for Objective-C methods.
It is undefined behavior if the class value is null and the method is
not an Objective-C method.

witness_method
``````````````
::

  sil-instruction ::= 'witness_method' sil-method-attributes?
                        sil-type ',' sil-decl-ref ':' sil-type

  %1 = witness_method $T, #Proto.method!1 \
    : $@convention(witness_method) <Self: Proto> U -> V
  // $T must be an archetype
  // #Proto.method!1 must be a reference to a method of one of the protocol
  //   constraints on T
  // <Self: Proto> U -> V must be the type of the referenced method,
  //   generic on Self
  // %1 will be of type $@convention(thin) <Self: Proto> U -> V

Looks up the implementation of a protocol method for a generic type variable
constrained by that protocol. The result will be generic on the ``Self``
archetype of the original protocol and have the ``witness_method`` calling
convention. If the referenced protocol is an ``@objc`` protocol, the
resulting type has the ``objc`` calling convention.

dynamic_method
``````````````
::

  sil-instruction ::= 'dynamic_method' sil-method-attributes?
                      sil-operand ',' sil-decl-ref ':' sil-type

  %1 = dynamic_method %0 : $P, #X.method!1 : $@convention(thin) U -> V
  // %0 must be of a protocol or protocol composition type $P,
  // where $P contains the Swift.DynamicLookup protocol
  // #X.method!1 must be a reference to an @objc method of any class
  // or protocol type
  //
  // The "self" argument of the method type $@convention(thin) U -> V must be
  //   Builtin.UnknownObject

Looks up the implementation of an Objective-C method with the same
selector as the named method for the dynamic type of the
value inside an existential container. The "self" operand of the result
function value is represented using an opaque type, the value for which must
be projected out as a value of type ``Builtin.UnknownObject``.

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

  sil-instruction ::= 'apply' '[nothrow]'? sil-value
                        sil-apply-substitution-list?
                        '(' (sil-value (',' sil-value)*)? ')'
                        ':' sil-type

  sil-apply-substitution-list ::= '<' sil-substitution
                                      (',' sil-substitution)* '>'
  sil-substitution ::= type '=' type

  %r = apply %0(%1, %2, ...) : $(A, B, ...) -> R
  // Note that the type of the callee '%0' is specified *after* the arguments
  // %0 must be of a concrete function type $(A, B, ...) -> R
  // %1, %2, etc. must be of the argument types $A, $B, etc.
  // %r will be of the return type $R

  %r = apply %0<A, B>(%1, %2, ...) : $<T, U>(T, U, ...) -> R
  // %0 must be of a polymorphic function type $<T, U>(T, U, ...) -> R
  // %1, %2, etc. must be of the argument types after substitution $A, $B, etc.
  // %r will be of the substituted return type $R'

Transfers control to function ``%0``, passing it the given arguments. In
the instruction syntax, the type of the callee is specified after the argument
list; the types of the argument and of the defined value are derived from the
function type of the callee. The input argument tuple type is destructured,
and each element is passed as an individual argument. The ``apply``
instruction does no retaining or releasing of its arguments by itself; the
`calling convention`_'s retain/release policy must be handled by separate
explicit ``retain`` and ``release`` instructions. The return value will
likewise not be implicitly retained or released.

The callee value must have function type.  That function type may not
have an error result, except the instruction has the ``nothrow`` attribute set.
The ``nothrow`` attribute specifies that the callee has an error result but
does not actually throw.
For the regular case of calling a function with error result, use ``try_apply``.

NB: If the callee value is of a thick function type, ``apply`` currently
consumes the callee value at +1 strong retain count.

If the callee is generic, all of its generic parameters must be bound by the
given substitution list. The arguments and return value is
given with these generic substitutions applied.

partial_apply
`````````````
::

  sil-instruction ::= 'partial_apply' callee-ownership-attr? sil-value
                        sil-apply-substitution-list?
                        '(' (sil-value (',' sil-value)*)? ')'
                        ':' sil-type
  callee-ownership-attr ::= '[callee_guaranteed]'

  %c = partial_apply %0(%1, %2, ...) : $(Z..., A, B, ...) -> R
  // Note that the type of the callee '%0' is specified *after* the arguments
  // %0 must be of a concrete function type $(Z..., A, B, ...) -> R
  // %1, %2, etc. must be of the argument types $A, $B, etc.,
  //   of the tail part of the argument tuple of %0
  // %c will be of the partially-applied thick function type (Z...) -> R

  %c = partial_apply %0<A, B>(%1, %2, ...) : $(Z..., T, U, ...) -> R
  // %0 must be of a polymorphic function type $<T, U>(T, U, ...) -> R
  // %1, %2, etc. must be of the argument types after substitution $A, $B, etc.
  //   of the tail part of the argument tuple of %0
  // %r will be of the substituted thick function type $(Z'...) -> R'

Creates a closure by partially applying the function ``%0`` to a partial
sequence of its arguments. In the instruction syntax, the type of the callee is
specified after the argument list; the types of the argument and of the defined
value are derived from the function type of the callee. The closure context will
be allocated with retain count 1 and initialized to contain the values ``%1``,
``%2``, etc.  The closed-over values will not be retained; that must be done
separately before the ``partial_apply``. The closure does however take
ownership of the partially applied arguments; when the closure reference
count reaches zero, the contained values will be destroyed.

If the callee is generic, all of its generic parameters must be bound by the
given substitution list. The arguments are given with these generic
substitutions applied, and the resulting closure is of concrete function
type with the given substitutions applied. The generic parameters themselves
cannot be partially applied; all of them must be bound. The result is always
a concrete function.

TODO: The instruction, when applied to a generic function,
currently implicitly performs abstraction difference transformations enabled
by the given substitutions, such as promoting address-only arguments and returns
to register arguments. This should be fixed.

By default, ``partial_apply`` creates a closure whose invocation takes ownership
of the context, meaning that a call implicitly releases the closure. The
``[callee_guaranteed]`` change this to a caller-guaranteed model, where the
caller promises not to release the closure while the function is being called.

This instruction is used to implement both curry thunks and closures. A
curried function in Swift::

  func foo(_ a:A)(b:B)(c:C)(d:D) -> E { /* body of foo */ }

emits curry thunks in SIL as follows (retains and releases omitted for
clarity)::

  func @foo : $@convention(thin) A -> B -> C -> D -> E {
  entry(%a : $A):
    %foo_1 = function_ref @foo_1 : $@convention(thin) (B, A) -> C -> D -> E
    %thunk = partial_apply %foo_1(%a) : $@convention(thin) (B, A) -> C -> D -> E
    return %thunk : $B -> C -> D -> E
  }

  func @foo_1 : $@convention(thin) (B, A) -> C -> D -> E {
  entry(%b : $B, %a : $A):
    %foo_2 = function_ref @foo_2 : $@convention(thin) (C, B, A) -> D -> E
    %thunk = partial_apply %foo_2(%b, %a) \
      : $@convention(thin) (C, B, A) -> D -> E
    return %thunk : $(B, A) -> C -> D -> E
  }

  func @foo_2 : $@convention(thin) (C, B, A) -> D -> E {
  entry(%c : $C, %b : $B, %a : $A):
    %foo_3 = function_ref @foo_3 : $@convention(thin) (D, C, B, A) -> E
    %thunk = partial_apply %foo_3(%c, %b, %a) \
      : $@convention(thin) (D, C, B, A) -> E
    return %thunk : $(C, B, A) -> D -> E
  }

  func @foo_3 : $@convention(thin) (D, C, B, A) -> E {
  entry(%d : $D, %c : $C, %b : $B, %a : $A):
    // ... body of foo ...
  }

A local function in Swift that captures context, such as ``bar`` in the
following example::

  func foo(_ x:Int) -> Int {
    func bar(_ y:Int) -> Int {
      return x + y
    }
    return bar(1)
  }

lowers to an uncurried entry point and is curried in the enclosing function::

  func @bar : $@convention(thin) (Int, @box Int, *Int) -> Int {
  entry(%y : $Int, %x_box : $@box Int, %x_address : $*Int):
    // ... body of bar ...
  }

  func @foo : $@convention(thin) Int -> Int {
  entry(%x : $Int):
    // Create a box for the 'x' variable
    %x_box = alloc_box $Int
    %x_addr = project_box %x_box : $@box Int
    store %x to %x_addr : $*Int

    // Create the bar closure
    %bar_uncurried = function_ref @bar : $(Int, Int) -> Int
    %bar = partial_apply %bar_uncurried(%x_box, %x_addr) \
      : $(Int, Builtin.NativeObject, *Int) -> Int

    // Apply it
    %1 = integer_literal $Int, 1
    %ret = apply %bar(%1) : $(Int) -> Int

    // Clean up
    release %bar : $(Int) -> Int
    return %ret : $Int
  }

builtin
```````
::

  sil-instruction ::= 'builtin' string-literal
                        sil-apply-substitution-list?
                        '(' (sil-operand (',' sil-operand)*)? ')'
                        ':' sil-type

  %1 = builtin "foo"(%1 : $T, %2 : $U) : $V
  // "foo" must name a function in the Builtin module

Invokes functionality built into the backend code generator, such as LLVM-
level instructions and intrinsics.

Metatypes
~~~~~~~~~

These instructions access metatypes, either statically by type name or
dynamically by introspecting class or generic values.

metatype
````````
::

  sil-instruction ::= 'metatype' sil-type

  %1 = metatype $T.Type
  // %1 has type $T.Type

Creates a reference to the metatype object for type ``T``.

value_metatype
``````````````
::

  sil-instruction ::= 'value_metatype' sil-type ',' sil-operand

  %1 = value_metatype $T.Type, %0 : $T
  // %0 must be a value or address of type $T
  // %1 will be of type $T.Type

Obtains a reference to the dynamic metatype of the value ``%0``.

existential_metatype
````````````````````
::

  sil-instruction ::= 'existential_metatype' sil-type ',' sil-operand

  %1 = existential_metatype $P.Type, %0 : $P
  // %0 must be a value of class protocol or protocol composition
  //   type $P, or an address of address-only protocol type $*P
  // %1 will be a $P.Type value referencing the metatype of the
  //   concrete value inside %0

Obtains the metatype of the concrete value
referenced by the existential container referenced by ``%0``.

objc_protocol
`````````````
::

  sil-instruction ::= 'objc_protocol' protocol-decl : sil-type

  %0 = objc_protocol #ObjCProto : $Protocol

*TODO* Fill this in.

Aggregate Types
~~~~~~~~~~~~~~~

These instructions construct and project elements from structs, tuples, and
class instances.

retain_value
````````````

::

  sil-instruction ::= 'retain_value' sil-operand

  retain_value %0 : $A

Retains a loadable value, which simply retains any references it holds.

For trivial types, this is a no-op.  For reference types, this is equivalent to
a ``strong_retain``.  For ``@unowned`` types, this is equivalent to an
``unowned_retain``.  In each of these cases, those are the preferred forms.

For aggregate types, especially enums, it is typically both easier
and more efficient to reason about aggregate copies than it is to
reason about copies of the subobjects.

unmanaged_retain_value
``````````````````````

::

  sil-instruction ::= 'unmanaged_retain_value' sil-value

  unmanaged_retain_value %0 : $A

This instruction has the same local semantics as ``retain_value`` but:

* Is valid in ownership qualified SIL.
* Is not intended to be statically paired at compile time by the compiler.

The intention is that this instruction is used to implement unmanaged
constructs.

copy_value
``````````

::

   sil-instruction ::= 'copy_value' sil-operand

   %1 = copy_value %0 : $A

Performs a copy of a loadable value as if by the value's type lowering and
returns the copy. The returned copy semantically is a value that is completely
independent of the operand. In terms of specific types:

1. For trivial types, this is equivalent to just propagating through the trivial
   value.
2. For reference types, this is equivalent to performing a ``strong_retain``
   operation and returning the reference.
3. For ``@unowned`` types, this is equivalent to performing an
   ``unowned_retain`` and returning the operand.
4. For aggregate types, this is equivalent to recursively performing a
   ``copy_value`` on its components, forming a new aggregate from the copied
   components, and then returning the new aggregate.

In ownership qualified functions, a ``copy_value`` produces a +1 value that must
be consumed at most once along any path through the program.

release_value
`````````````

::

  sil-instruction ::= 'release_value' sil-operand

  release_value %0 : $A

Destroys a loadable value, by releasing any retainable pointers within it.

This is defined to be equivalent to storing the operand into a stack
allocation and using 'destroy_addr' to destroy the object there.

For trivial types, this is a no-op.  For reference types, this is
equivalent to a ``strong_release``.  For ``@unowned`` types, this is
equivalent to an ``unowned_release``.  In each of these cases, those
are the preferred forms.

For aggregate types, especially enums, it is typically both easier
and more efficient to reason about aggregate destroys than it is to
reason about destroys of the subobjects.

unmanaged_release_value
```````````````````````

::

  sil-instruction ::= 'unmanaged_release_value' sil-value

  unmanaged_release_value %0 : $A

This instruction has the same local semantics as ``release_value`` but:

* Is valid in ownership qualified SIL.
* Is not intended to be statically paired at compile time by the compiler.

The intention is that this instruction is used to implement unmanaged
constructs.

destroy_value
`````````````

::

  sil-instruction ::= 'destroy_value' sil-operand

  destroy_value %0 : $A

Destroys a loadable value, by releasing any retainable pointers within it.

This is defined to be equivalent to storing the operand into a stack
allocation and using 'destroy_addr' to destroy the object there.

For trivial types, this is a no-op.  For reference types, this is
equivalent to a ``strong_release``.  For ``@unowned`` types, this is
equivalent to an ``unowned_release``.  In each of these cases, those
are the preferred forms.

For aggregate types, especially enums, it is typically both easier
and more efficient to reason about aggregate destroys than it is to
reason about destroys of the subobjects.

autorelease_value
`````````````````

::

  sil-instruction ::= 'autorelease_value' sil-operand

  autorelease_value %0 : $A

*TODO* Complete this section.

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

ref_tail_addr
`````````````
::

  sil-instruction ::= 'ref_tail_addr' sil-operand ',' sil-type

  %1 = ref_tail_addr %0 : $C, $E
  // %0 must be a value of class type $C with tail-allocated elements $E
  // %1 will be of type $*E

Given an instance of a class, which is created with tail-allocated array(s),
derives the address of the first element of the first tail-allocated array.
This instruction is used to project the first tail-allocated element from an
object which is created by an ``alloc_ref`` with ``tail_elems``.
It is undefined behavior if the class instance does not have tail-allocated
arrays or if the element-types do not match.

Enums
~~~~~

These instructions construct values of enum type. Loadable enum values are
created with the `enum`_ instruction. Address-only enums require two-step
initialization. First, if the case requires data, that data is stored into
the enum at the address projected by `init_enum_data_addr`_. This step is
skipped for cases without data. Finally, the tag for
the enum is injected with an `inject_enum_addr`_ instruction::

  enum AddressOnlyEnum {
    case HasData(AddressOnlyType)
    case NoData
  }

  sil @init_with_data : $(AddressOnlyType) -> AddressOnlyEnum {
  entry(%0 : $*AddressOnlyEnum, %1 : $*AddressOnlyType):
    // Store the data argument for the case.
    %2 = init_enum_data_addr %0 : $*AddressOnlyEnum, #AddressOnlyEnum.HasData!enumelt.1
    copy_addr [take] %2 to [initialization] %1 : $*AddressOnlyType
    // Inject the tag.
    inject_enum_addr %0 : $*AddressOnlyEnum, #AddressOnlyEnum.HasData!enumelt.1
    return
  }

  sil @init_without_data : $() -> AddressOnlyEnum {
    // No data. We only need to inject the tag.
    inject_enum_addr %0 : $*AddressOnlyEnum, #AddressOnlyEnum.NoData!enumelt
    return
  }

Accessing the value of a loadable enum is inseparable from dispatching on its
discriminator and is done with the `switch_enum`_ terminator::

  enum Foo { case A(Int), B(String) }

  sil @switch_foo : $(Foo) -> () {
  entry(%foo : $Foo):
    switch_enum %foo : $Foo, case #Foo.A!enumelt.1: a_dest, case #Foo.B!enumelt.1: b_dest

  a_dest(%a : $Int):
    /* use %a */

  b_dest(%b : $String):
    /* use %b */
  }

An address-only enum can be tested by branching on it using the
`switch_enum_addr`_ terminator. Its value can then be taken by destructively
projecting the enum value with `unchecked_take_enum_data_addr`_::

  enum Foo<T> { case A(T), B(String) }

  sil @switch_foo : $<T> (Foo<T>) -> () {
  entry(%foo : $*Foo<T>):
    switch_enum_addr %foo : $*Foo<T>, case #Foo.A!enumelt.1: a_dest, \
      case #Foo.B!enumelt.1: b_dest

  a_dest:
    %a = unchecked_take_enum_data_addr %foo : $*Foo<T>, #Foo.A!enumelt.1
    /* use %a */

  b_dest:
    %b = unchecked_take_enum_data_addr %foo : $*Foo<T>, #Foo.B!enumelt.1
    /* use %b */
  }

enum
````
::

  sil-instruction ::= 'enum' sil-type ',' sil-decl-ref (',' sil-operand)?

  %1 = enum $U, #U.EmptyCase!enumelt
  %1 = enum $U, #U.DataCase!enumelt.1, %0 : $T
  // $U must be an enum type
  // #U.DataCase or #U.EmptyCase must be a case of enum $U
  // If #U.Case has a data type $T, %0 must be a value of type $T
  // If #U.Case has no data type, the operand must be omitted
  // %1 will be of type $U

Creates a loadable enum value in the given ``case``. If the ``case`` has a
data type, the enum value will contain the operand value.

unchecked_enum_data
```````````````````
::

  sil-instruction ::= 'unchecked_enum_data' sil-operand ',' sil-decl-ref

  %1 = unchecked_enum_data %0 : $U, #U.DataCase!enumelt.1
  // $U must be an enum type
  // #U.DataCase must be a case of enum $U with data
  // %1 will be of object type $T for the data type of case U.DataCase

Unsafely extracts the payload data for an enum ``case`` from an enum value.
It is undefined behavior if the enum does not contain a value of the given
case.

init_enum_data_addr
```````````````````
::

  sil-instruction ::= 'init_enum_data_addr' sil-operand ',' sil-decl-ref

  %1 = init_enum_data_addr %0 : $*U, #U.DataCase!enumelt.1
  // $U must be an enum type
  // #U.DataCase must be a case of enum $U with data
  // %1 will be of address type $*T for the data type of case U.DataCase

Projects the address of the data for an enum ``case`` inside an enum. This
does not modify the enum or check its value. It is intended to be used as
part of the initialization sequence for an address-only enum. Storing to
the ``init_enum_data_addr`` for a case followed by ``inject_enum_addr`` with that
same case is guaranteed to result in a fully-initialized enum value of that
case being stored. Loading from the ``init_enum_data_addr`` of an initialized
enum value or injecting a mismatched case tag is undefined behavior.

The address is invalidated as soon as the operand enum is fully initialized by
an ``inject_enum_addr``.

inject_enum_addr
````````````````
::

  sil-instruction ::= 'inject_enum_addr' sil-operand ',' sil-decl-ref

  inject_enum_addr %0 : $*U, #U.Case!enumelt
  // $U must be an enum type
  // #U.Case must be a case of enum $U
  // %0 will be overlaid with the tag for #U.Case

Initializes the enum value referenced by the given address by overlaying the
tag for the given case. If the case has no data, this instruction is sufficient
to initialize the enum value. If the case has data, the data must be stored
into the enum at the ``init_enum_data_addr`` address for the case *before*
``inject_enum_addr`` is applied. It is undefined behavior if
``inject_enum_addr`` is applied for a case with data to an uninitialized enum,
or if ``inject_enum_addr`` is applied for a case with data when data for a
mismatched case has been stored to the enum.

unchecked_take_enum_data_addr
`````````````````````````````
::

  sil-instruction ::= 'unchecked_take_enum_data_addr' sil-operand ',' sil-decl-ref

  %1 = unchecked_take_enum_data_addr %0 : $*U, #U.DataCase!enumelt.1
  // $U must be an enum type
  // #U.DataCase must be a case of enum $U with data
  // %1 will be of address type $*T for the data type of case U.DataCase

Invalidates an enum value, and takes the address of the payload for the given
enum ``case`` in-place in memory. The referenced enum value is no longer valid,
but the payload value referenced by the result address is valid and must be
destroyed. It is undefined behavior if the referenced enum does not contain a
value of the given ``case``. The result shares memory with the original enum
value; the enum memory cannot be reinitialized as an enum until the payload has
also been invalidated.

(1.0 only)

For the first payloaded case of an enum, ``unchecked_take_enum_data_addr``
is guaranteed to have no side effects; the enum value will not be invalidated.

select_enum
```````````
::

  sil-instruction ::= 'select_enum' sil-operand sil-select-case*
                      (',' 'default' sil-value)?
                      ':' sil-type

  %n = select_enum %0 : $U,      \
    case #U.Case1!enumelt: %1,           \
    case #U.Case2!enumelt: %2, /* ... */ \
    default %3 : $T

  // $U must be an enum type
  // #U.Case1, Case2, etc. must be cases of enum $U
  // %1, %2, %3, etc. must have type $T
  // %n has type $T

Selects one of the "case" or "default" operands based on the case of an
enum value. This is equivalent to a trivial `switch_enum`_ branch sequence::

  entry:
    switch_enum %0 : $U,            \
      case #U.Case1!enumelt: bb1,           \
      case #U.Case2!enumelt: bb2, /* ... */ \
      default bb_default
  bb1:
    br cont(%1 : $T) // value for #U.Case1
  bb2:
    br cont(%2 : $T) // value for #U.Case2
  bb_default:
    br cont(%3 : $T) // value for default
  cont(%n : $T):
    // use argument %n

but turns the control flow dependency into a data flow dependency.
For address-only enums, `select_enum_addr`_ offers the same functionality for
an indirectly referenced enum value in memory.

select_enum_addr
````````````````
::

  sil-instruction ::= 'select_enum_addr' sil-operand sil-select-case*
                      (',' 'default' sil-value)?
                      ':' sil-type

  %n = select_enum_addr %0 : $*U,      \
    case #U.Case1!enumelt: %1,           \
    case #U.Case2!enumelt: %2, /* ... */ \
    default %3 : $T

  // %0 must be the address of an enum type $*U
  // #U.Case1, Case2, etc. must be cases of enum $U
  // %1, %2, %3, etc. must have type $T
  // %n has type $T

Selects one of the "case" or "default" operands based on the case of the
referenced enum value. This is the address-only counterpart to
`select_enum`_.

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

Depending on the constraints applied to an existential type, an existential
container may use one of several representations:

- **Opaque existential containers**: If none of the protocols in a protocol
  type are class protocols, then the existential container for that type is
  address-only and referred to in the implementation as an *opaque existential
  container*. The value semantics of the existential container propagate to the
  contained concrete value. Applying ``copy_addr`` to an opaque existential
  container copies the contained concrete value, deallocating or reallocating
  the destination container's owned buffer if necessary. Applying
  ``destroy_addr`` to an opaque existential container destroys the concrete
  value and deallocates any buffers owned by the existential container. The
  following instructions manipulate opaque existential containers:

  * `init_existential_addr`_
  * `open_existential_addr`_
  * `deinit_existential_addr`_

- **Opaque existential containers loadable types**: In the SIL Opaque Values
  mode of operation, we take an opaque value as-is.
  Said value might be replaced with one of the _addr instructions above
  before IR generation.
  The following instructions manipulate "loadable" opaque existential containers:
  
  * `init_existential_opaque`_
  * `open_existential_opaque`_
  * `deinit_existential_opaque`_

- **Class existential containers**: If a protocol type is constrained by one or
  more class protocols, then the existential container for that type is
  loadable and referred to in the implementation as a *class existential
  container*. Class existential containers have reference semantics and can be
  ``retain``-ed and ``release``-d. The following instructions manipulate class
  existential containers:

  * `init_existential_ref`_
  * `open_existential_ref`_

- **Metatype existential containers**: Existential metatypes use a
  container consisting of the type metadata for the conforming type along with
  the protocol conformances. Metatype existential containers are trivial types.
  The following instructions manipulate metatype existential containers:

  * `init_existential_metatype`_
  * `open_existential_metatype`_

- **Boxed existential containers**: The standard library ``Error`` protocol
  uses a size-optimized reference-counted container, which indirectly stores
  the conforming value. Boxed existential containers can be ``retain``-ed
  and ``release``-d. The following instructions manipulate boxed existential
  containers:

  * `alloc_existential_box`_
  * `project_existential_box`_
  * `open_existential_box`_
  * `dealloc_existential_box`_

Some existential types may additionally support specialized representations
when they contain certain known concrete types. For example, when Objective-C
interop is available, the ``Error`` protocol existential supports
a class existential container representation for ``NSError`` objects, so it
can be initialized from one using ``init_existential_ref`` instead of the
more expensive ``alloc_existential_box``::

  bb(%nserror: $NSError):
    // The slow general way to form an Error, allocating a box and
    // storing to its value buffer:
    %error1 = alloc_existential_box $Error, $NSError
    %addr = project_existential_box $NSError in %error1 : $Error
    strong_retain %nserror: $NSError
    store %nserror to %addr : $NSError

    // The fast path supported for NSError:
    strong_retain %nserror: $NSError
    %error2 = init_existential_ref %nserror: $NSError, $Error

init_existential_addr
`````````````````````
::

  sil-instruction ::= 'init_existential_addr' sil-operand ',' sil-type

  %1 = init_existential_addr %0 : $*P, $T
  // %0 must be of a $*P address type for non-class protocol or protocol
  //   composition type P
  // $T must be an AST type that fulfills protocol(s) P
  // %1 will be of type $*T', where T' is the maximally abstract lowering
  //    of type T

Partially initializes the memory referenced by ``%0`` with an existential
container prepared to contain a value of type ``$T``. The result of the
instruction is an address referencing the storage for the contained value, which
remains uninitialized. The contained value must be ``store``-d or
``copy_addr``-ed to in order for the existential value to be fully initialized.
If the existential container needs to be destroyed while the contained value
is uninitialized, ``deinit_existential_addr`` must be used to do so. A fully
initialized existential container can be destroyed with ``destroy_addr`` as
usual. It is undefined behavior to ``destroy_addr`` a partially-initialized
existential container.

init_existential_opaque
```````````````````````
::

  sil-instruction ::= 'init_existential_opaque' sil-operand ':' sil-type ','
                                             sil-type

  %1 = init_existential_opaque %0 : $L' : $C, $P
  // %0 must be of loadable type $L', lowered from AST type $L, conforming to
  //    protocol(s) $P
  // %1 will be of type $P

Loadable version of the above: Inits-up the existential
container prepared to contain a value of type ``$P``.

deinit_existential_addr
```````````````````````
::

  sil-instruction ::= 'deinit_existential_addr' sil-operand

  deinit_existential_addr %0 : $*P
  // %0 must be of a $*P address type for non-class protocol or protocol
  // composition type P

Undoes the partial initialization performed by
``init_existential_addr``.  ``deinit_existential_addr`` is only valid for
existential containers that have been partially initialized by
``init_existential_addr`` but haven't had their contained value initialized.
A fully initialized existential must be destroyed with ``destroy_addr``.

deinit_existential_opaque
`````````````````````````
::

  sil-instruction ::= 'deinit_existential_opaque' sil-operand

  deinit_existential_opaque %0 : $P
  // %0 must be of a $P opaque type for non-class protocol or protocol
  // composition type P

Undoes the partial initialization performed by
``init_existential_opaque``.  ``deinit_existential_opaque`` is only valid for
existential containers that have been partially initialized by
``init_existential_opaque`` but haven't had their contained value initialized.
A fully initialized existential must be destroyed with ``destroy_value``.

open_existential_addr
`````````````````````
::

  sil-instruction ::= 'open_existential_addr' sil-allowed-access sil-operand 'to' sil-type
  sil-allowed-access ::= 'immutable_access'
  sil-allowed-access ::= 'mutable_access'

  %1 = open_existential_addr immutable_access %0 : $*P to $*@opened P
  // %0 must be of a $*P type for non-class protocol or protocol composition
  //   type P
  // $*@opened P must be a unique archetype that refers to an opened
  // existential type P.
  // %1 will be of type $*P

Obtains the address of the concrete value inside the existential
container referenced by ``%0``. The protocol conformances associated
with this existential container are associated directly with the
archetype ``$*@opened P``. This pointer can be used with any operation
on archetypes, such as ``witness_method`` assuming this operation obeys the
access constraint: The returned address can either allow ``mutable_access`` or
``immutable_access``. Users of the returned address may only consume
(e.g ``destroy_addr`` or ``copy_addr [take]``) or mutate the value at the
address if they have ``mutable_access``.

open_existential_opaque
```````````````````````
::

  sil-instruction ::= 'open_existential_opaque' sil-operand 'to' sil-type

  %1 = open_existential_opaque %0 : $P to $@opened P
  // %0 must be of a $P type for non-class protocol or protocol composition
  //   type P
  // $@opened P must be a unique archetype that refers to an opened
  // existential type P.
  // %1 will be of type $P

Loadable version of the above: Opens-up the existential
container associated with ``%0``. The protocol conformances associated
with this existential container are associated directly with the
archetype ``$@opened P``.

init_existential_ref
````````````````````
::

  sil-instruction ::= 'init_existential_ref' sil-operand ':' sil-type ','
                                             sil-type

  %1 = init_existential_ref %0 : $C' : $C, $P
  // %0 must be of class type $C', lowered from AST type $C, conforming to
  //    protocol(s) $P
  // $P must be a class protocol or protocol composition type
  // %1 will be of type $P

Creates a class existential container of type ``$P`` containing a reference to
the class instance ``%0``.

open_existential_ref
````````````````````
::

  sil-instruction ::= 'open_existential_ref' sil-operand 'to' sil-type

  %1 = open_existential_ref %0 : $P to $@opened P
  // %0 must be of a $P type for a class protocol or protocol composition
  // $@opened P must be a unique archetype that refers to an opened
  //   existential type P
  // %1 will be of type $@opened P

Extracts the class instance reference from a class existential
container. The protocol conformances associated with this existential
container are associated directly with the archetype ``@opened P``. This
pointer can be used with any operation on archetypes, such as
``witness_method``. When the operand is of metatype type, the result
will be the metatype of the opened archetype.

init_existential_metatype
`````````````````````````
::

  sil-instruction ::= 'init_existential_metatype' sil-operand ',' sil-type

  %1 = init_existential_metatype $0 : $@<rep> T.Type, $@<rep> P.Type
  // %0 must be of a metatype type $@<rep> T.Type where T: P
  // %@<rep> P.Type must be the existential metatype of a protocol or protocol
  //    composition, with the same metatype representation <rep>
  // %1 will be of type $@<rep> P.Type

Creates a metatype existential container of type ``$P.Type`` containing the
conforming metatype of ``$T``.

open_existential_metatype
`````````````````````````
::

  sil-instruction ::= 'open_existential_metatype' sil-operand 'to' sil-type

  %1 = open_existential_metatype %0 : $@<rep> P.Type to $@<rep> (@opened P).Type
  // %0 must be of a $P.Type existential metatype for a protocol or protocol
  //    composition
  // $@<rep> (@opened P).Type must be the metatype of a unique archetype that
  //   refers to an opened existential type P, with the same metatype
  //   representation <rep>
  // %1 will be of type $@<rep> (@opened P).Type

Extracts the metatype from an existential metatype. The protocol conformances associated with this existential
container are associated directly with the archetype ``@opened P``.

alloc_existential_box
`````````````````````
::

  sil-instruction ::= 'alloc_existential_box' sil-type ',' sil-type

  %1 = alloc_existential_box $P, $T
  // $P must be a protocol or protocol composition type with boxed
  //   representation
  // $T must be an AST type that conforms to P
  // %1 will be of type $P
  // %1#1 will be of type $*T', where T' is the most abstracted lowering of T

Allocates a boxed existential container of type ``$P`` with space to hold a
value of type ``$T'``. The box is not fully initialized until a valid value
has been stored into the box. If the box must be deallocated before it is
fully initialized, ``dealloc_existential_box`` must be used. A fully
initialized box can be ``retain``-ed and ``release``-d like any
reference-counted type.  The ``project_existential_box`` instruction is used
to retrieve the address of the value inside the container.

project_existential_box
```````````````````````
::

  sil-instruction ::= 'project_existential_box' sil-type 'in' sil-operand

  %1 = project_existential_box $T in %0 : $P
  // %0 must be a value of boxed protocol or protocol composition type $P
  // $T must be the most abstracted lowering of the AST type for which the box
  // was allocated
  // %1 will be of type $*T

Projects the address of the value inside a boxed existential container.
The address is dependent on the lifetime of the owner reference ``%0``.
It is undefined behavior if the concrete type ``$T`` is not the same type for
which the box was allocated with ``alloc_existential_box``.

open_existential_box
````````````````````
::

  sil-instruction ::= 'open_existential_box' sil-operand 'to' sil-type

  %1 = open_existential_box %0 : $P to $*@opened P
  // %0 must be a value of boxed protocol or protocol composition type $P
  // %@opened P must be the address type of a unique archetype that refers to
  ///   an opened existential type P
  // %1 will be of type $*@opened P

Projects the address of the value inside a boxed existential container, and
uses the enclosed type and protocol conformance metadata to bind the
opened archetype ``$@opened P``. The result address is dependent on both
the owning box and the enclosing function; in order to "open" a boxed
existential that has directly adopted a class reference, temporary scratch
space may need to have been allocated.

dealloc_existential_box
```````````````````````
::

  sil-instruction ::= 'dealloc_existential_box' sil-operand, sil-type

  dealloc_existential_box %0 : $P, $T
  // %0 must be an uninitialized box of boxed existential container type $P
  // $T must be the AST type for which the box was allocated

Deallocates a boxed existential container. The value inside the existential
buffer is not destroyed; either the box must be uninitialized, or the value
must have been projected out and destroyed beforehand. It is undefined behavior
if the concrete type ``$T`` is not the same type for which the box was
allocated with ``alloc_existential_box``.

Blocks
~~~~~~

project_block_storage
`````````````````````
::

   sil-instruction ::= 'project_block_storage' sil-operand ':' sil-type

init_block_storage_header
`````````````````````````

*TODO* Fill this in. The printing of this instruction looks incomplete on trunk currently.


Unchecked Conversions
~~~~~~~~~~~~~~~~~~~~~

These instructions implement type conversions which are not checked. These are
either user-level conversions that are always safe and do not need to be
checked, or implementation detail conversions that are unchecked for
performance or flexibility.

upcast
``````
::

  sil-instruction ::= 'upcast' sil-operand 'to' sil-type

  %1 = upcast %0 : $D to $B
  // $D and $B must be class types or metatypes, with B a superclass of D
  // %1 will have type $B

Represents a conversion from a derived class instance or metatype to a
superclass, or from a base-class-constrained archetype to its base class.

address_to_pointer
``````````````````
::

  sil-instruction ::= 'address_to_pointer' sil-operand 'to' sil-type

  %1 = address_to_pointer %0 : $*T to $Builtin.RawPointer
  // %0 must be of an address type $*T
  // %1 will be of type Builtin.RawPointer

Creates a ``Builtin.RawPointer`` value corresponding to the address ``%0``.
Converting the result pointer back to an address of the same type will give
an address equivalent to ``%0``. It is undefined behavior to cast the
``RawPointer`` to any address type other than its original address type or
any `layout compatible types`_.

pointer_to_address
``````````````````
::

  sil-instruction ::= 'pointer_to_address' sil-operand 'to' ('[' 'strict' ']')? sil-type

  %1 = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*T
  // %1 will be of type $*T

Creates an address value corresponding to the ``Builtin.RawPointer`` value
``%0``.  Converting a ``RawPointer`` back to an address of the same type as
its originating ``address_to_pointer`` instruction gives back an equivalent
address. It is undefined behavior to cast the ``RawPointer`` back to any type
other than its original address type or `layout compatible types`_. It is
also undefined behavior to cast a ``RawPointer`` from a heap object to any
address type.

The ``strict`` flag indicates whether the returned address adheres to
strict aliasing.  If true, then the type of each memory access
dependent on this address must be consistent with the memory's bound
type. A memory access from an address that is not strict cannot have
its address substituted with a strict address, even if other nearby
memory accesses at the same location are strict.


unchecked_ref_cast
``````````````````
::

  sil-instruction ::= 'unchecked_ref_cast' sil-operand 'to' sil-type

  %1 = unchecked_ref_cast %0 : $A to $B
  // %0 must be an object of type $A
  // $A must be a type with retainable pointer representation
  // %1 will be of type $B
  // $B must be a type with retainable pointer representation

Converts a heap object reference to another heap object reference
type. This conversion is unchecked, and it is undefined behavior if
the destination type is not a valid type for the heap object. The heap
object reference on either side of the cast may be a class
existential, and may be wrapped in one level of Optional.

unchecked_ref_cast_addr
```````````````````````
::

  sil-instruction ::= 'unchecked_ref_cast_addr'
                      sil-type 'in' sil-operand 'to'
                      sil-type 'in' sil-operand

  unchecked_ref_cast_addr $A in %0 : $*A to $B in %1 : $*B
  // %0 must be the address of an object of type $A
  // $A must be a type with retainable pointer representation
  // %1 must be the address of storage for an object of type $B
  // $B must be a retainable pointer representation

Loads a heap object reference from an address and stores it at the
address of another uninitialized heap object reference. The loaded
reference is always taken, and the stored reference is
initialized. This conversion is unchecked, and it is undefined
behavior if the destination type is not a valid type for the heap
object. The heap object reference on either side of the cast may be a
class existential, and may be wrapped in one level of Optional.

unchecked_addr_cast
```````````````````
::

  sil-instruction ::= 'unchecked_addr_cast' sil-operand 'to' sil-type

  %1 = unchecked_addr_cast %0 : $*A to $*B
  // %0 must be an address
  // %1 will be of type $*B

Converts an address to a different address type. Using the resulting
address is undefined unless ``B`` is layout compatible with ``A``. The
layout of ``B`` may be smaller than that of ``A`` as long as the lower
order bytes have identical layout.

unchecked_trivial_bit_cast
``````````````````````````

::

  sil-instruction ::= 'unchecked_trivial_bit_cast' sil-operand 'to' sil-type

  %1 = unchecked_trivial_bit_cast %0 : $Builtin.NativeObject to $Builtin.Word
  // %0 must be an object.
  // %1 must be an object with trivial type.

Bitcasts an object of type ``A`` to be of same sized or smaller type
``B`` with the constraint that ``B`` must be trivial. This can be used
for bitcasting among trivial types, but more importantly is a one way
bitcast from non-trivial types to trivial types.

unchecked_bitwise_cast
``````````````````````
::

   sil-instruction ::= 'unchecked_bitwise_cast' sil-operand 'to' sil-type

   %1 = unchecked_bitwise_cast %0 : $A to $B

Bitwise copies an object of type ``A`` into a new object of type ``B``
of the same size or smaller.

ref_to_raw_pointer
``````````````````
::

  sil-instruction ::= 'ref_to_raw_pointer' sil-operand 'to' sil-type

  %1 = ref_to_raw_pointer %0 : $C to $Builtin.RawPointer
  // $C must be a class type, or Builtin.NativeObject, or Builtin.UnknownObject
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
  // $C must be a class type, or Builtin.NativeObject, or Builtin.UnknownObject
  // %1 will be of type $C

Converts a ``Builtin.RawPointer`` back to a heap object reference. Casting
a heap object reference to ``Builtin.RawPointer`` back to the same type gives
an equivalent heap object reference (though the raw pointer has no ownership
semantics for the object on its own). It is undefined behavior to cast a
``RawPointer`` to a type unrelated to the dynamic type of the heap object.
It is also undefined behavior to cast a ``RawPointer`` from an address to any
heap object type.

ref_to_unowned
``````````````

::

  sil-instruction ::= 'ref_to_unowned' sil-operand

  %1 = unowned_to_ref %0 : T
  // $T must be a reference type
  // %1 will have type $@unowned T

Adds the ``@unowned`` qualifier to the type of a reference to a heap
object.  No runtime effect.

unowned_to_ref
``````````````

::

  sil-instruction ::= 'unowned_to_ref' sil-operand

  %1 = unowned_to_ref %0 : $@unowned T
  // $T must be a reference type
  // %1 will have type $T

Strips the ``@unowned`` qualifier off the type of a reference to a
heap object.  No runtime effect.

ref_to_unmanaged
````````````````

TODO

unmanaged_to_ref
````````````````

TODO


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
  ``(a:Int, b:Float, UnicodeScalar) -> ()`` and ``(x:Int, Float, z:UnicodeScalar) -> ()`` are
  ABI compatible.

- A class tuple element of the destination type may be a superclass or
  subclass of the source type's corresponding tuple element.

The function types may also differ in attributes, except that the
``convention`` attribute cannot be changed.

thin_function_to_pointer
````````````````````````

TODO

pointer_to_thin_function
````````````````````````

TODO

ref_to_bridge_object
````````````````````
::

  sil-instruction ::= 'ref_to_bridge_object' sil-operand, sil-operand

  %2 = ref_to_bridge_object %0 : $C, %1 : $Builtin.Word
  // %1 must be of reference type $C
  // %2 will be of type Builtin.BridgeObject

Creates a ``Builtin.BridgeObject`` that references ``%0``, with spare bits
in the pointer representation populated by bitwise-OR-ing in the value of
``%1``. It is undefined behavior if this bitwise OR operation affects the
reference identity of ``%0``; in other words, after the following instruction
sequence::

  %b = ref_to_bridge_object %r : $C, %w : $Builtin.Word
  %r2 = bridge_object_to_ref %b : $Builtin.BridgeObject to $C

``%r`` and ``%r2`` must be equivalent. In particular, it is assumed that
retaining or releasing the ``BridgeObject`` is equivalent to retaining or
releasing the original reference, and that the above ``ref_to_bridge_object``
/ ``bridge_object_to_ref`` round-trip can be folded away to a no-op.

On platforms with ObjC interop, there is additionally a platform-specific
bit in the pointer representation of a ``BridgeObject`` that is reserved to
indicate whether the referenced object has native Swift refcounting. It is
undefined behavior to set this bit when the first operand references an
Objective-C object.

bridge_object_to_ref
````````````````````
::

  sil-instruction ::= 'bridge_object_to_ref' sil-operand 'to' sil-type

  %1 = bridge_object_to_ref %0 : $Builtin.BridgeObject to $C
  // $C must be a reference type
  // %1 will be of type $C

Extracts the object reference from a ``Builtin.BridgeObject``, masking out any
spare bits.

bridge_object_to_word
`````````````````````
::

  sil-instruction ::= 'bridge_object_to_word' sil-operand 'to' sil-type

  %1 = bridge_object_to_word %0 : $Builtin.BridgeObject to $Builtin.Word
  // %1 will be of type $Builtin.Word

Provides the bit pattern of a ``Builtin.BridgeObject`` as an integer.

thin_to_thick_function
``````````````````````
::

  sil-instruction ::= 'thin_to_thick_function' sil-operand 'to' sil-type

  %1 = thin_to_thick_function %0 : $@convention(thin) T -> U to $T -> U
  // %0 must be of a thin function type $@convention(thin) T -> U
  // The destination type must be the corresponding thick function type
  // %1 will be of type $T -> U

Converts a thin function value, that is, a bare function pointer with no
context information, into a thick function value with ignored context.
Applying the resulting thick function value is equivalent to applying the
original thin value. The ``thin_to_thick_function`` conversion may be
eliminated if the context is proven not to be needed.

thick_to_objc_metatype
``````````````````````
::

  sil-instruction ::= 'thick_to_objc_metatype' sil-operand 'to' sil-type

  %1 = thick_to_objc_metatype %0 : $@thick T.Type to $@objc_metatype T.Type
  // %0 must be of a thick metatype type $@thick T.Type
  // The destination type must be the corresponding Objective-C metatype type
  // %1 will be of type $@objc_metatype T.Type

Converts a thick metatype to an Objective-C class metatype. ``T`` must
be of class, class protocol, or class protocol composition type.

objc_to_thick_metatype
``````````````````````
::

  sil-instruction ::= 'objc_to_thick_metatype' sil-operand 'to' sil-type

  %1 = objc_to_thick_metatype %0 : $@objc_metatype T.Type to $@thick T.Type
  // %0 must be of an Objective-C metatype type $@objc_metatype T.Type
  // The destination type must be the corresponding thick metatype type
  // %1 will be of type $@thick T.Type

Converts an Objective-C class metatype to a thick metatype. ``T`` must
be of class, class protocol, or class protocol composition type.

objc_metatype_to_object
```````````````````````

TODO

objc_existential_metatype_to_object
```````````````````````````````````

TODO

is_nonnull
``````````
::

  sil-instruction ::= 'is_nonnull' sil-operand

  %1 = is_nonnull %0 : $C
  // %0 must be of reference or function type $C
  // %1 will be of type Builtin.Int1

Checks whether a reference type value is null, returning 1 if
the value is not null, or 0 if it is null.  If the value is a function
type, it checks the function pointer (not the data pointer) for null.

This is not a sensical thing for SIL to represent given that reference
types are non-nullable, but makes sense at the machine level.  This is
a horrible hack that should go away someday.

Checked Conversions
~~~~~~~~~~~~~~~~~~~

Some user-level cast operations can fail and thus require runtime checking.

The `unconditional_checked_cast_addr`_, `unconditional_checked_cast_value`_ and `unconditional_checked_cast`_
instructions performs an unconditional checked cast; it is a runtime failure
if the cast fails. The `checked_cast_addr_br`_, `checked_cast_value_br`_ and `checked_cast_br`_
terminator instruction performs a conditional checked cast; it branches to one
of two destinations based on whether the cast succeeds or not.

unconditional_checked_cast
``````````````````````````
::

  sil-instruction ::= 'unconditional_checked_cast' sil-operand 'to' sil-type

  %1 = unconditional_checked_cast %0 : $A to $B
  %1 = unconditional_checked_cast %0 : $*A to $*B
  // $A and $B must be both objects or both addresses
  // %1 will be of type $B or $*B

Performs a checked scalar conversion, causing a runtime failure if the
conversion fails.

unconditional_checked_cast_addr
```````````````````````````````
::

  sil-instruction ::= 'unconditional_checked_cast_addr'
                       sil-cast-consumption-kind
                       sil-type 'in' sil-operand 'to'
                       sil-type 'in' sil-operand
  sil-cast-consumption-kind ::= 'take_always'
  sil-cast-consumption-kind ::= 'take_on_success'
  sil-cast-consumption-kind ::= 'copy_on_success'

  %1 = unconditional_checked_cast_addr take_on_success $A in %0 : $*@thick A to $B in $*@thick B
  // $A and $B must be both addresses
  // %1 will be of type $*B

Performs a checked indirect conversion, causing a runtime failure if the
conversion fails.

unconditional_checked_cast_value
````````````````````````````````
::

  sil-instruction ::= 'unconditional_checked_cast_value' sil-operand 'to' sil-type

  %1 = unconditional_checked_cast_value %0 : $A to $B
  // $A must be not be an address
  // $B must be an opaque value
  // %1 will be of type $B

Performs a checked conversion, causing a runtime failure if the
conversion fails.

Runtime Failures
~~~~~~~~~~~~~~~~

cond_fail
`````````
::

  sil-instruction ::= 'cond_fail' sil-operand

  cond_fail %0 : $Builtin.Int1
  // %0 must be of type $Builtin.Int1

This instruction produces a `runtime failure`_ if the operand is one.
Execution proceeds normally if the operand is zero.

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
no-return function.

return
``````
::

  sil-terminator ::= 'return' sil-operand

  return %0 : $T
  // $T must be the return type of the current function

Exits the current function and returns control to the calling function. If
the current function was invoked with an ``apply`` instruction, the result
of that function will be the operand of this ``return`` instruction. If
the current function was invoked with a ``try_apply` instruction, control
resumes at the normal destination, and the value of the basic block argument
will be the operand of this ``return`` instruction.

``return`` does not retain or release its operand or any other values.

A function must not contain more than one ``return`` instruction.

throw
`````
::

  sil-terminator ::= 'throw' sil-operand

  throw %0 : $T
  // $T must be the error result type of the current function

Exits the current function and returns control to the calling
function. The current function must have an error result, and so the
function must have been invoked with a ``try_apply` instruction.
Control will resume in the error destination of that instruction, and
the basic block argument will be the operand of the ``throw``.

``throw`` does not retain or release its operand or any other values.

A function must not contain more than one ``throw`` instruction.

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

cond_br
``````````
::

  sil-terminator ::= 'cond_br' sil-operand ','
                       sil-identifier '(' (sil-operand (',' sil-operand)*)? ')' ','
                       sil-identifier '(' (sil-operand (',' sil-operand)*)? ')'

  cond_br %0 : $Builtin.Int1, true_label (%a : $A, %b : $B, ...), \
                                 false_label (%x : $X, %y : $Y, ...)
  // %0 must be of $Builtin.Int1 type
  // `true_label` and `false_label` must refer to block labels within the
  //   current function and must not be identical
  // %a, %b, etc. must be of the types of `true_label`'s arguments
  // %x, %y, etc. must be of the types of `false_label`'s arguments

Conditionally branches to ``true_label`` if ``%0`` is equal to ``1`` or to
``false_label`` if ``%0`` is equal to ``0``, binding the corresponding set of
values to the arguments of the chosen destination block.

switch_value
````````````
::

  sil-terminator ::= 'switch_value' sil-operand
                       (',' sil-switch-value-case)*
                       (',' sil-switch-default)?
  sil-switch-value-case ::= 'case' sil-value ':' sil-identifier
  sil-switch-default ::= 'default' sil-identifier

  switch_value %0 : $Builtin.Int<n>, case %1: label1, \
                                     case %2: label2, \
                                     ...,            \
                                     default labelN

  // %0 must be a value of builtin integer type $Builtin.Int<n>
  // `label1` through `labelN` must refer to block labels within the current
  //   function
  // FIXME: All destination labels currently must take no arguments

Conditionally branches to one of several destination basic blocks based on a
value of builtin integer or function type. If the operand value matches one of the ``case``
values of the instruction, control is transferred to the corresponding basic
block. If there is a ``default`` basic block, control is transferred to it if
the value does not match any of the ``case`` values. It is undefined behavior
if the value does not match any cases and no ``default`` branch is provided.

select_value
````````````
::

  sil-instruction ::= 'select_value' sil-operand sil-select-value-case*
                      (',' 'default' sil-value)?
                      ':' sil-type
  sil-select-value-case ::= 'case' sil-value ':' sil-value


  %n = select_value %0 : $U, \
    case %c1: %r1,           \
    case %c2: %r2, /* ... */ \
    default %r3 : $T

  // $U must be a builtin type. Only integers types are supported currently.
  // c1, c2, etc must be of type $U
  // %r1, %r2, %r3, etc. must have type $T
  // %n has type $T

Selects one of the "case" or "default" operands based on the case of a
value. This is equivalent to a trivial `switch_value`_ branch sequence::

  entry:
    switch_value %0 : $U,            \
      case %c1: bb1,           \
      case %c2: bb2, /* ... */ \
      default bb_default
  bb1:
    br cont(%r1 : $T) // value for %c1
  bb2:
    br cont(%r2 : $T) // value for %c2
  bb_default:
    br cont(%r3 : $T) // value for default
  cont(%n : $T):
    // use argument %n

but turns the control flow dependency into a data flow dependency.

switch_enum
```````````
::

  sil-terminator ::= 'switch_enum' sil-operand
                       (',' sil-switch-enum-case)*
                       (',' sil-switch-default)?
  sil-switch-enum-case ::= 'case' sil-decl-ref ':' sil-identifier

  switch_enum %0 : $U, case #U.Foo!enumelt: label1, \
                        case #U.Bar!enumelt: label2, \
                        ...,                 \
                        default labelN

  // %0 must be a value of enum type $U
  // #U.Foo, #U.Bar, etc. must be 'case' declarations inside $U
  // `label1` through `labelN` must refer to block labels within the current
  //   function
  // label1 must take either no basic block arguments, or a single argument
  //   of the type of #U.Foo's data
  // label2 must take either no basic block arguments, or a single argument
  //   of the type of #U.Bar's data, etc.
  // labelN must take no basic block arguments

Conditionally branches to one of several destination basic blocks based on the
discriminator in a loadable ``enum`` value. Unlike ``switch_int``,
``switch_enum`` requires coverage of the operand type: If the ``enum`` type
is resilient, the ``default`` branch is required; if the ``enum`` type is
fragile, the ``default`` branch is required unless a destination is assigned to
every ``case`` of the ``enum``. The destination basic block for a ``case`` may
take an argument of the corresponding ``enum`` ``case``'s data type (or of the
address type, if the operand is an address). If the branch is taken, the
destination's argument will be bound to the associated data inside the
original enum value.  For example::

  enum Foo {
    case Nothing
    case OneInt(Int)
    case TwoInts(Int, Int)
  }

  sil @sum_of_foo : $Foo -> Int {
  entry(%x : $Foo):
    switch_enum %x : $Foo,       \
      case #Foo.Nothing!enumelt: nothing, \
      case #Foo.OneInt!enumelt.1:  one_int, \
      case #Foo.TwoInts!enumelt.1: two_ints

  nothing:
    %zero = integer_literal $Int, 0
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

On a path dominated by a destination block of ``switch_enum``, copying or
destroying the basic block argument has equivalent reference counting semantics
to copying or destroying the ``switch_enum`` operand::

    // This retain_value...
    retain_value %e1 : $Enum
    switch_enum %e1, case #Enum.A: a, case #Enum.B: b
  a(%a : $A):
    // ...is balanced by this release_value
    release_value %a
  b(%b : $B):
    // ...and this one
    release_value %b

switch_enum_addr
````````````````
::

  sil-terminator ::= 'switch_enum_addr' sil-operand
                       (',' sil-switch-enum-case)*
                       (',' sil-switch-default)?

  switch_enum_addr %0 : $*U, case #U.Foo!enumelt: label1, \
                                          case #U.Bar!enumelt: label2, \
                                          ...,                 \
                                          default labelN

  // %0 must be the address of an enum type $*U
  // #U.Foo, #U.Bar, etc. must be cases of $U
  // `label1` through `labelN` must refer to block labels within the current
  //   function
  // The destinations must take no basic block arguments

Conditionally branches to one of several destination basic blocks based on
the discriminator in the enum value referenced by the address operand.

Unlike ``switch_int``, ``switch_enum`` requires coverage of the operand type:
If the ``enum`` type is resilient, the ``default`` branch is required; if the
``enum`` type is fragile, the ``default`` branch is required unless a
destination is assigned to every ``case`` of the ``enum``.
Unlike ``switch_enum``, the payload value is not passed to the destination
basic blocks; it must be projected out separately with `unchecked_take_enum_data_addr`_.

dynamic_method_br
`````````````````
::

  sil-terminator ::= 'dynamic_method_br' sil-operand ',' sil-decl-ref
                       ',' sil-identifier ',' sil-identifier

  dynamic_method_br %0 : $P, #X.method!1, bb1, bb2
  // %0 must be of protocol type
  // #X.method!1 must be a reference to an @objc method of any class
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

checked_cast_br
```````````````
::

  sil-terminator ::= 'checked_cast_br' sil-checked-cast-exact?
                      sil-operand 'to' sil-type ','
                      sil-identifier ',' sil-identifier
  sil-checked-cast-exact ::= '[' 'exact' ']'

  checked_cast_br %0 : $A to $B, bb1, bb2
  checked_cast_br %0 : $*A to $*B, bb1, bb2
  checked_cast_br [exact] %0 : $A to $A, bb1, bb2
  // $A and $B must be both object types or both address types
  // bb1 must take a single argument of type $B or $*B
  // bb2 must take no arguments

Performs a checked scalar conversion from ``$A`` to ``$B``. If the conversion
succeeds, control is transferred to ``bb1``, and the result of the cast is
passed into ``bb1`` as an argument. If the conversion fails, control is
transferred to ``bb2``.

An exact cast checks whether the dynamic type is exactly the target
type, not any possible subtype of it.  The source and target types
must be class types.

checked_cast_value_br
`````````````````````
::

  sil-terminator ::= 'checked_cast_value_br'
                      sil-operand 'to' sil-type ','
                      sil-identifier ',' sil-identifier
  sil-checked-cast-exact ::= '[' 'exact' ']'

  checked_cast_value_br %0 : $A to $B, bb1, bb2
  // $A must be not be an address
  // $B must be an opaque value
  // bb1 must take a single argument of type $B
  // bb2 must take no arguments

Performs a checked opaque conversion from ``$A`` to ``$B``. If the conversion
succeeds, control is transferred to ``bb1``, and the result of the cast is
passed into ``bb1`` as an argument. If the conversion fails, control is
transferred to ``bb2``.

checked_cast_addr_br
````````````````````
::

  sil-terminator ::= 'checked_cast_addr_br'
                      sil-cast-consumption-kind
                      sil-type 'in' sil-operand 'to'
                      sil-stype 'in' sil-operand ','
                      sil-identifier ',' sil-identifier
  sil-cast-consumption-kind ::= 'take_always'
  sil-cast-consumption-kind ::= 'take_on_success'
  sil-cast-consumption-kind ::= 'copy_on_success'

  checked_cast_addr_br take_always $A in %0 : $*@thick A to $B in %2 : $*@thick B, bb1, bb2
  // $A and $B must be both address types
  // bb1 must take a single argument of type $*B
  // bb2 must take no arguments

Performs a checked indirect conversion from ``$A`` to ``$B``. If the
conversion succeeds, control is transferred to ``bb1``, and the result of the
cast is left in the destination. If the conversion fails, control is
transferred to ``bb2``.

try_apply
`````````
::

  sil-terminator ::= 'try_apply' sil-value
                        sil-apply-substitution-list?
                        '(' (sil-value (',' sil-value)*)? ')'
                        ':' sil-type
    'normal' sil-identifier, 'error' sil-identifier

  try_apply %0(%1, %2, ...) : $(A, B, ...) -> (R, @error E),
    normal bb1, error bb2
  bb1(%3 : R):
  bb2(%4 : E):

  // Note that the type of the callee '%0' is specified *after* the arguments
  // %0 must be of a concrete function type $(A, B, ...) -> (R, @error E)
  // %1, %2, etc. must be of the argument types $A, $B, etc.

Transfers control to the function specified by ``%0``, passing it the
given arguments.  When ``%0`` returns, control resumes in either the
normal destination (if it returns with ``return``) or the error
destination (if it returns with ``throw``).

``%0`` must have a function type with an error result.

The rules on generic substitutions are identical to those of ``apply``.

Assertion configuration
~~~~~~~~~~~~~~~~~~~~~~~

To be able to support disabling assertions at compile time there is a builtin
``assertion_configuration`` function. A call to this function can be replaced at
compile time by a constant or can stay opaque.

All calls to the ``assert_configuration`` function are replaced by the constant
propagation pass to the appropriate constant depending on compile time settings.
Subsequent passes remove dependent unwanted control flow. Using this mechanism
we support conditionally enabling/disabling of code in SIL libraries depending
on the assertion configuration selected when the library is linked into user
code.

There are three assertion configurations: Debug (0), Release (1) and
DisableReplacement (-1).

The optimization flag or a special assert configuration flag determines the
value. Depending on the configuration value assertions in the standard library
will be executed or not.

The standard library uses this builtin to define an assert that can be
disabled at compile time.

::

  func assert(...) {
    if (Int32(Builtin.assert_configuration()) == 0) {
      _fatal_error_message(message, ...)
    }
  }

The ``assert_configuration`` function application is serialized when we build
the standard library (we recognize the ``-parse-stdlib`` option and don't do the
constant replacement but leave the function application to be serialized to
sil).

The compiler flag that influences the value of the ``assert_configuration``
function application is the optimization flag: at ``-Onone` the application will
be replaced by ``Debug`` at higher optimization levels the instruction will be
replaced by ``Release``. Optionally, the value to use for replacement can be
specified with the ``-assert-config`` flag which overwrites the value selected by
the optimization flag (possible values are ``Debug``, ``Release``,
``DisableReplacement``).

If the call to the ``assert_configuration`` function stays opaque until IRGen,
IRGen will replace the application by the constant representing Debug mode (0).
This happens we can build the standard library .dylib. The generate sil will
retain the function call but the generated .dylib will contain code with
assertions enabled.
