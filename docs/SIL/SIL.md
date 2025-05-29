# Introduction

SIL is an SSA-form IR with high-level semantic information designed to
implement the Swift programming language.

In contrast to LLVM IR, SIL is a generally target-independent format
representation that can be used for code distribution, but it can also
express target-specific concepts as well as LLVM can.

SIL has three representations:

- In memory: SIL is represented by data structures which are implemented in the
  compiler sources. Optimization passes use the in-memory representation of SIL.

- Textual: The compiler and related utilities can print and parse textual SIL
  files. Textual SIL files have the file extension `.sil`.

- Binary: SIL can be stored and read from binary files. Binary SIL files are
  called "swift-module" files and have the extension `.swiftmodule`.
  Note that the binary format is not stable. Swift-module files are _not_
  compatible between compiler versions.

In this document SIL is explained by describing and using the syntax of the textual representation.

SIL is reliant on Swift's type system and declarations, so textual SIL syntax
is an extension of Swift's. A `.sil` file is a Swift source file with
added SIL definitions. The Swift source is parsed only for its
declarations; Swift `func` bodies (except for nested declarations) and
top-level code are ignored by the SIL parser. In a `.sil` file, there
are no implicit imports; the `Swift` and/or `Builtin` standard modules
must be imported explicitly if used.

## SIL in the Swift Compiler

At a high level, the Swift compiler follows a strict pipeline
architecture:

-   The _parser_ constructs an AST from Swift source code.
-   The _type checker_ (SEMA) type-checks the AST and annotates it with type
    information.
-   _SILGen_ generates *raw SIL* from an AST. In raw SIL dataflow requirements,
    such as definitive assignment, function returns, etc. are not yet been
    enforced.
-   A series of _mandatory optimization passes_ are run over the raw SIL both
    to perform required optimizations and to emit language-specific diagnostics.
    These are always run, regardless of the selected optimization mode,
    and produce *canonical SIL*.
-   General *optimization passes* run over the canonical
    SIL to improve performance of the resulting executable. These are
    enabled and controlled by the optimization level.
-   *IRGen* lowers canonical SIL to LLVM IR.
-   The LLVM backend applies LLVM optimizations, runs the
    LLVM code generator and emits binary code.

## Ownership SSA

Ownership SSA - or OSSA - is an augmented version of SSA that enforces
ownership invariants for SSA values in SIL functions. OSSA allows to verify
ownership. 

By using these ownership invariants, SIL in OSSA form can be validated
statically as not containing use after free errors or leaked memory. This
allows the compiler at compile time to detect bugs in SILGen and optimization
passes.

SILGen generates OSSA and OSSA is maintained throughout mandatory optimizations
and for part of optimization passes. At some point in the SIL pass pipeline
OSSA is "lowered" to plain SSA. From this point on no ownership verification
can be done anymore.

# SIL Stages

SIL files declare the processing stage of the included SIL with one of
the declarations `sil_stage raw` or `sil_stage canonical` at top level.
Only one such declaration may appear in a file.

```
decl ::= sil-stage-decl
sil-stage-decl ::= 'sil_stage' sil-stage

sil-stage ::= 'raw'
sil-stage ::= 'canonical'
```

There are different invariants on SIL depending on what stage of
processing has been applied to it.

-   **Raw SIL** is the form produced by SILGen that has not been run
    through mandatory optimizations or diagnostic passes. Raw SIL may
    not have a fully-constructed SSA graph. It may contain dataflow
    errors, like not all variables are initialized.
    Some instructions may be represented in non-canonical forms,
    such as `assign`. Raw SIL cannot be used for native code generation or
    distribution.
-   **Canonical SIL** is SIL as it exists after mandatory optimizations
    and diagnostics. Dataflow errors must be eliminated, and certain
    instructions must be canonicalized to simpler forms. Performance
    optimization and native code generation are derived from this form,
    and a module can be distributed containing SIL in this forms.

# SIL Types

SIL types are introduced with the `$` sigil. SIL's type system is
closely related to Swift's, and so the type after the `$` is parsed
largely according to Swift's type grammar.

```
sil-type ::= '$' '*'? generic-parameter-list? type
```

## Formal vs. Lowered Types

A formal type corresponds to a Swift type as it is defined in the source code.
The AST and the type checker work with formal types. Formal types can be
canonicalized which resolves type aliases and removes sugar. Later stages of
the compiler, like the SIL optimizer, only deal with canonical formal types.
Therefore, if we speak about formal types in the following sections, we always
refer to _canonical_ formal types.

Each formal type has a corresponding lowered type. However, most lowered types
are identical to their original formal type, for example all nominal types,
like classes, structs or enums (except `Optional`). Only a few kind of types
are lowered to a different lowered type. The most prominent example is function
types: a lowered function type adds information about the calling convention and 
it lowers tuple arguments to individual arguments.

For example, the formal type of

```
  func foo(a: (Int, String), b: any P) { }
```

is `((Int, String), any P) -> ()` whereas its lowered type is
`(Int, @guaranteed String, @in_guaranteed any P) -> ()`.

Deriving a lowered type from a formal type is called _type lowering_ which is
described in detail in the [Types](Types.md#Type-Lowering) document.

SIL types are always lowered types. The soundness of the SIL type system
depends on lowered types and the SIL optimizer needs lowered types to perform
correct optimizations.

However, there are a few places where SIL needs to refer to formal types. These
are operations on types which are "user visible", for example cast
instructions.

For example, a cast from `((Int, Bool)) -> ()` to `(Int, Bool) -> ()` fails
because the two formal function types differ. If a SIL cast instruction would
operate on SIL types, the cast would incorrectly succeed because the formal
types of those functions types are equivalent.

To summarize:

|                    | Definition                                   | Example                             |
| ------------------ | -------------------------------------------- | ----------------------------------- |
| **Formal type**    | original type from the source code           | `typealias C = ((Int, Bool)) -> ()` |
| **Canonical type** | formal type minus sugar, aliases resolved    | `((Int, Bool)) -> ()`               |
| **SIL type**       | lowered canonical type, plus is-address flag | `$*(Int, Bool) -> ()`               |


## Loadable vs. Address-only Types

Most SIL types are _loadable_. That means that a value of such a type can be
represented as an SSA value.  If a type is not loadable, it is address-only.
For example, if the compiler does not know the layout size of a type, the type
is address-only.

Values of address-only types can only live in memory and can only be accessed
via the address to the memory location.

> **_Note:_** that in future, SIL might represent address-only types as SSA values.

## Address vs. Object Types

The type of a value in SIL is either:
-   an *object type* `$T`, where `T` is a loadable type, or
-   an *address type* `$*T`, where `T` is a loadable or address-only type.

The *address of T* `$*T` is a pointer to memory containing a value of type
`$T`. This can be the address of a stack location
([`alloc_stack`](Instructions.md#alloc_stack), an internal pointer into a class
instance or reference-counted box, the address of a global variable, an
indirect function argument or an address produced from an unsafe pointer.
Addresses of loadable types can be loaded and stored to access values of those
types.

Addresses of address-only types can only be used with
instructions that manipulate their operands indirectly by address, such
as `copy_addr` or `destroy_addr`, or as arguments to functions. It is
illegal to have a value of type `$T` if `T` is address-only.

Addresses are not reference-counted pointers like class values are. They
cannot be retained or released.

Address types are not *first-class*: they cannot appear in recursive positions
in type expressions, e.g. the address of an address cannot be directly taken.
For example, the type `$**T` is not a legal type.

Addresses can be passed as arguments to functions if the corresponding
parameter is indirect. They cannot be returned, except by a `begin_apply`.

## Trivial vs. Non-Trivial Types

A *trivial type* is a loadable type with trivial value semantics.  Values of
trivial type can be copied or destroyed without any "extra effort" like retain
or release operations. For example, `Int` or `Bool` are trivial types.

In contrast, a non-trivial type requires extra code to be emitted to perform
copies and destroys. Some common reasons why a type is non-trivial are:

- The type (or a sub-type contained within it) contains a class reference which
  requiring retain/release operations.

- The type is not known at compile time, for example, a generic type or a
  resilient type.

- The type is a non-copyable type with a custom `deinit` that must be run when
  a value of such a type is destroyed.

> For more information on types in SIL see the [Types](Types.md) document.

# Functions

SIL functions are defined with the `sil` keyword. SIL function names are
introduced with the `@` sigil.
A SIL function name will become the LLVM IR name for the function, and is usually
the mangled name of the originating Swift declaration. The `sil` syntax
declares the function's name and SIL type, and defines the body of the
function inside braces. The declared type must be a function type, which
may be generic.

```
decl ::= sil-function
sil-function ::= 'sil' sil-linkage? sil-function-attribute+
                   sil-function-name ':' sil-type
                   '{' effects* sil-basic-block* '}'
sil-function-name ::= '@' [A-Za-z_0-9]+
```

If a function body does not contain at least one `sil-basic-block`, the
function is an external declaration.

For the list of function attributes see [FunctionAttributes](FunctionAttributes.md).

## Effects

Optionally a function can define a list of effects, which describe effects,
like memory effects or escaping effects for arguments. For details
see the documentation in
[Effects.swift](`SwiftCompilerSources/Sources/SIL/Effects.swift`).

```
effects ::= '[' argument-name ':' argument-effect (',' argument-effect)*]'
effects ::= '[' 'global' ':' global-effect (',' global-effect)*]'
argument-name ::= '%' [0-9]+

argument-effect ::= 'noescape' defined-effect? projection-path?
argument-effect ::= 'escape' defined-effect? projection-path? '=>' arg-or-return  // exclusive escape
argument-effect ::= 'escape' defined-effect? projection-path? '->' arg-or-return  // not-exclusive escape
argument-effect ::= side-effect

global-effect ::= 'traps'
global-effect ::= 'allocate'
global-effect ::= 'deinit_barrier'
global-effect ::= side-effect

side-effect ::= 'read' projection-path?
side-effect ::= 'write' projection-path?
side-effect ::= 'copy' projection-path?
side-effect ::= 'destroy' projection-path?

arg-or-return ::= argument-name ('.' projection-path)?
arg-or-return ::= '%r' ('.' projection-path)?
defined-effect ::= '!'    // the effect is defined in the source code and not
                          // derived by the optimizer

projection-path ::= path-component ('.' path-component)* 
path-component ::= 's' [0-9]+        // struct field
path-component ::= 'c' [0-9]+        // class field
path-component ::= 'ct'              // class tail element
path-component ::= 'e' [0-9]+        // enum case
path-component ::= [0-9]+            // tuple element
path-component ::= 'v**'             // any value fields
path-component ::= 'c*'              // any class field
path-component ::= '**'              // anything
```

Note that even function _declarations_, i.e. functions without basic blocks,
can defined effects. For example:

```
sil @foo : $@convention(thin) (@guaranteed String) -> String {
[%0: escape v** => %r.v**]
[global: read,copy]
}
```

# Basic Blocks

A function body consists of one or more basic blocks. Each basic block contains
one or more instructions and can have arguments. The last instruction of a
block is a "terminator" instruction.  The function's entry point is always the
first basic block in its body.

```
sil-basic-block ::= sil-label sil-instruction-def* sil-terminator
sil-label ::= sil-identifier ('(' sil-argument (',' sil-argument)* ')')? ':'
sil-argument-ownership-kind ::= @owned
sil-argument-ownership-kind ::= @guaranteed
sil-argument-ownership-kind ::= @reborrow
sil-argument ::= sil-value-name ':' sil-argument-ownership-kind? sil-type

sil-instruction-result ::= sil-value-name
sil-instruction-result ::= '(' (sil-value-name (',' sil-value-name)*)? ')'
sil-instruction-source-info ::= (',' sil-scope-ref)? (',' sil-loc)?
sil-instruction-def ::=
  (sil-instruction-result '=')? sil-instruction sil-instruction-source-info
```

For a detailed description of all instructions, see the [Instruction
Reference](Instructions.md).

## Basic Block Arguments

A block argument is a [value](#values-and-operands) and can have an ownership
kind specified before its type annotation.  
There are three kind of arguments:

- Function arguments: Per definition, the arguments of the first basic block in
  the function (the entry block) are the function's arguments. The types of the
  function arguments must match the parameters of the function type. Function
  arguments are bound by the function's caller:

```
    sil @foo : $@convention(thin) (Int, @guaranteed String) -> () {
    bb0(%0 : $Int, %1 : @guaranteed $String):
      ...
```

- Terminator results: Some terminator instructions forward their result to the
  successor block's arguments. For example a
  [`switch_enum`](Instructions.md#switch_enum) forwards the payload of an enum
  case to the corresponding successor block's argument.

```
      ...
      switch_enum %1, case #Optional.some!enumelt: bb1, default: bb2
    bb1(%term_result : $Int):
      ...
```

- Phi arguments: If a block has multiple predecessor blocks, the terminators of
  the predecessor blocks must be `br` or `cond_br` instructions and the
  operand values of those branch instructions are passed to the block's
  arguments. This corresponds to LLVM's phi nodes. Basic block arguments are
  bound by the branch from the predecessor block:

```
      cond_br %cond, bb1, bb2
    bb1:
      br bb3(%1)
    bb2:
      br bb3(%2)
    bb3(%phi : $Builtin.Int):
      return %phi
```

When a function is in Ownership SSA, arguments additionally have an
explicit annotated convention that describe the ownership semantics of
the argument value:

```
sil [ossa] @baz : $@convention(thin) (@owned String, @guaranteed String) -> () {
bb0(%0 : @owned $String, %1 : @guaranteed $String):
  ...
```

# Values and Operands

SIL values are introduced with the `%` sigil and named by an
alphanumeric identifier, which references the instruction result or basic block
argument that produces the value. SIL values may also refer to the
keyword 'undef', which is a value of undefined contents.

```
sil-identifier ::= [A-Za-z_0-9]+
sil-value-name ::= '%' sil-identifier
sil-value ::= sil-value-name
sil-value ::= 'undef'
sil-operand ::= sil-value
sil-operand ::= sil-value ':' sil-type
```

Values are used in instruction operands. Unlike LLVM IR, SIL instructions that
take value operands *only* accept value operands. 
References to literal constants, functions, global variables, or other entities
are introduced as the results of dedicated instructions such as
`integer_literal`, `function_ref`, and `global_addr`.

There are several kind of values in SIL:
- Basic block arguments
- Instruction results: most instructions have a single result. A few
  instructions have multiple results.
- `undef`

## Ownership

> **_Note:_** Many more details of OSSA are described in the [Ownership](Ownership.md) document.

In OSSA each non-trivial SIL value is statically mapped to an _ownership kind_:

- `@owned`: A value that exists independently of any other value and is
  consumed exactly once along all control flow paths through a function.
  "Consuming" the value means that it is either destroyed (via `destroy_value`)
  or consumed by a consuming instruction (e.g. stored to memory with `store`).

```
    sil [ossa] @foo : $@convention(thin) (@owned String, @inout String) -> () {
    bb0(%0 : @owned $String, %1 : $*String):
      cond_br %cond, bb1, bb2
    bb1:
      destroy_value %0        // "consumed" by destroying
      ...
    bb2:
      store %0 to [assign] %1 // consumed by storing to memory
```

- `@guaranteed`: A value with a scoped lifetime whose liveness is dependent on
  the lifetime of some other "enclosing" value. Due to this lifetime
  dependence, the enclosing value is required to be statically live over the
  entire scope where the guaranteed value is valid.

```
    bb0(%0 : @owned $String):
      %1 = begin_borrow %0   // %1 is borrowed from the enclosing value %0
      ...
      end_borrow %1          // %0 must be kept alive until here
      destroy_value %0
```

-  `@unowned`: A value that is only guaranteed to be instantaneously
    valid and must be copied before the value is used in an `@owned` or
    `@guaranteed` context. This is needed both to model argument values
    with the ObjC unsafe unowned argument convention and also to model
    the ownership resulting from bit-casting a trivial type to a
    non-trivial type. An unowned value should never be consumed.

Trivial values (like `Int`) values of address types don't have an ownership
kind associated.

The ownership kind of a value is statically determined:

- Basic block arguments have their ownership explicitly specified: 

```
    bb1(%0 : @owned $String, %1 : @guaranteed $String, %2 : $Int):
      ...
```

- The ownership of most instruction results can be statically determined from
  the instruction's kind and the offset of the value in the result tuple. For
  example `copy_value` has only one result and that result is always an owned
  value, whereas `begin_borrow` always produces a guaranteed value.

- Forwarding instructions: some instructions work with both, owned and
  guaranteed ownership, and "forward" the ownership from their operand(s) to
  their result(s), for example cast instructions.  
  If a forwarding instruction has multiple operands (like `struct` or `tuple`),
  the ownership of all operand values must be consistent. The operands values
  cannot have both guaranteed and owned ownership. However, it's possible to
  mix operands with ownership with trivial operands.

  ```
      bb1(%1 : @owned $A, %2 : @guaranteed $A, %3 : $Int):
        %4 = struct $S1 (%1, %3)   // owned
        %5 = struct $S1 (%2, %3)   // guaranteed
        %6 = struct $S2 (%1, %2)   // ERROR!
  ```

### Lifetimes

Lifetimes have following properties:

- A lifetime begins with a single definition - the "producer" of the value.

- There are many possibilities how an _owned_ value can be produced; either by
  an _owned_ block argument or by one of the many kind of instructions which have
  an _owned_ result value.

```
    bb0(%0: @owned $C):
      %1 = apply %f() : @convention(thin) () -> @owned D
      %2 = copy_value %x
      %3 = load [copy] %a
      ...
```

- Lifetimes of guaranteed values are called _borrow scopes_. A borrow scope
  starts with a single definition - the borrow-introducer. There are only a few
  different kinds of borrow-introducers:
    - guaranteed function argument: the lifetime spans over the whole function
      and doesn't need a scope-ending use.
    - `borrowed-from` instruction: it produces a borrow scope from a [reborrow
      phi argument](#Phi-arguments)
    - `begin_borrow` instruction
    - `load_borrow` instruction
    - `begin_apply` instruction

- A lifetime of a value must end exactly once along all control flow paths
  starting from its definition to function exits. It is illegal to end the
  lifetime more than once on a control flow path ("over-consume") or to _not_ end
  the lifetime on a control flow path ("leak").

- There are many kinds of instructions which end the lifetime of an _owned_
  value, which is called a _consume_. For example, `destroy_value` destroys the
  value (a _destroying consume_); a function call consumes a value if it is
  passed to a consuming argument, etc.

- A lifetime of a _guaranteed_ value  - a borrow scope - can only end at
  following instructions:
    - `end_borrow`
    - `end_apply` - in case the borrow introducer is a `begin_apply`
    - `br` instruction, which is the incoming value of a [reborrow phi
      argument] (#Phi-arguments).
    - guaranteed function arguments don't have a lifetime-ending instruction.

- Uses within a lifetime are called _non-consuming_ or _interior_ uses and must
  not appear _after_ the lifetime-ending consuming use ("use-after-consume") on
  all control flow paths. It depends on the kind of instruction if its operands
  are consuming or interior uses.

```
      %1 = load [copy] %0   // producer
      %2 = copy_value %1    // interior use of %1
      %3 = move_value %1    // consuming use of %1
      %4 = copy_value %1    // ERROR: use of %1 outside %1's lifetime
```

- Forwarding instructions end owned lifetimes and immediately produce a new
  owned value, which introduces a separate lifetime. The combined lifetimes over
  all forwarding instructions is called a _forward-extended_ lifetime.

```
      %1 = copy_value %0        -+               -+
      ...                        | lifetime       |
      // forwarding instruction  |                |
      %2 = struct $S (%1)       -+  -+            | forward-extended lifetime
                                     | lifetime   |
      // consuming instruction       |            |
      destroy_value %2              -+           -+
```

- Forwarding instructions do _not_ end the lifetime of guaranteed values.

```
      // borrow introducer
      %1 = begin_borrow                          -+
      ...                                         |
      // forwarding instruction                   | lifetime = borrow scope
      %2 = struct $S (%1)  // forwarded use =     |   
      ...                  //   interior use      |
      end_borrow %1                              -+
```

Note that forward-extended lifetimes don't necessarily have a single
definition. If a forwarding-lifetime contains forwarding aggregate instructions
or [phi arguments](#Phi-arguments) the forwarding-lifetime can have multiple
producers. For example:

```
  %1 = load [copy] %a1     // producer           -+
  %2 = load [copy] %a2     // producer            |  forward-extended lifetime
  %3 = struct $S (%1, %2)                         |
  %4 = destroy_value %3                          -+
```

Trivial values are never consumed and therefore don't have any restrictions on
where they are used on any control flow paths.

### Borrow Scopes

A borrow scope is a liverange of a guaranteed value which keeps its
[enclosing value](#Borrow-Introducers-and-Enclosing-Values)(s) alive.
It prevents optimizations from destroying an enclosing value before the borrow
scope has ended.
The most common borrow scope is a `begin_borrow`-`end_borrow` pair. But there
are other instructions which introduce borrow scopes:

#### `begin_borrow`

A [`begin_borrow`](Instructions.md#begin_borrow) defines a borrow scope for its
operand. The borrow scope ends at an `end_borrow`.

```
  %2 = begin_borrow %1          -+ borrow scope for %1
  // ...                         |
  end_borrow %2                 -+ %1 must be alive until here
```

Such a borrow scope can also "go through" a phi-argument. In this case the
overall borrow scope is the combination of multiple borrow lifetimes which are
"connected" by reborrow phi-arguments (see [Phi arguments](#phi-arguments)).

#### `load_borrow`

A [`load_borrow`](Instructions.md#load_borrow) is similar to `begin_borrow`,
except that its enclosing value is not an SSA value but a memory location.
During the scope of a `load_borrow`-`end_borrow` the memory location must not be mutated.

```
  %2 = load_borrow %addr    -+ borrow scope for memory value at %addr
  // ...                     |
  end_borrow %2             -+ memory at %addr must not be mutated until here
```

#### `store_borrow`

A [`store_borrow`](Instructions.md#store_borrow) is similar to `begin_borrow`,
except that beside defining a borrow scope it also stores the borrowed value
to a stack location. During the borrow scope (which ends at an `end_borrow`), the
borrowed value lives at the stack location.

```
  %s = alloc_stack $T
  %2 = store_borrow %1 to %s   -+ borrow scope for %1
  // ...                        |
  end_borrow %2                -+ %1 must be alive until here
  dealloc_stack %s
```

#### `partial_apply`

A [`partial_apply [on_stack]`](Instructions.md#partial_apply) defines borrow
scopes for its non-trivial arguments. The scope begins at the `partial_apply`
and ends at the end of the forward-extended lifetime of the `partial_apply`'s
result - the non-escaping closure.

```
  %3 = partial_apply [on_stack] %f(%1, %2)    -+ borrow scope for %1 and %2
  %4 = convert_function %3 to $SomeFuncType    |
  destroy_value %4                            -+ %1 and %2 must be alive until here
```
 
#### `mark_dependence`

A [`mark_dependence [nonescaping]`](Instructions.md#mark_dependence) defines a
borrow scope for its base operand. The scope begins at the `mark_dependence`
and ends at the forward-extended lifetime of the `mark_dependence`'s result.

```
  %3 = mark_dependence %2 on %1   -+ borrow scope for %1
  %4 = upcast %3 to $C             |
  destroy_value %4                -+ %1 must be alive until here
```

### Borrow Introducers and Enclosing Values

Every guaranteed value has a set of borrow-introducers (usually one), each of
which dominates the value and introduces a borrow scope that encloses all
forwarded uses of the guaranteed value.

```
  %1 = begin_borrow %0                // borrow introducer for %2, %3, %4
  %2 = begin_borrow %1                // borrow introducer for %3, %4
  %3 = tuple (%1, %2)                 // forwarded guaranteed value
  %4 = struct $S (%3)                 // forwarded guaranteed value
  end_borrow %2                       // end of borrow scope %2
  end_borrow %1                       // end of borrow scope %1
```

This example also shows that inner borrow scopes may be nested in outer borrow
scopes.

The _enclosing values_ of a forwarded guaranteed value are simply its
borrow-introducers.  Whereas the _enclosing values_ of a borrow-introducer
itself are the (owned or guaranteed) values from which the borrow scope is
derived, for example the operand of a `begin_borrow`.

```
                               Borrow Introducer    Enclosing Value
                               ~~~~~~~~~~~~~~~~~    ~~~~~~~~~~~~~~~
  %1 = load [copy] %0          -                    none
  %2 = begin_borrow %1         %2                   %1
  %3 = begin_borrow %2         %3                   %2
  %4 = struct $S (%3)          %3                   %3
```

Only the borrow-introducers `begin_borrow` and `borrowed-from` have enclosing
values. The set of enclosing values is empty for guaranteed function arguments,
`load_borrow` and `begin_apply`.

### Phi arguments

The lifetimes of owned and guaranteed values can end at branch (`br`)
instructions. Branch instructions define the incoming values for phi-arguments
in their target block. We distinguish between three kinds of ownership of
non-trivial phi arguments:

- `@owned`: The lifetimes of the incoming values end at the predecessor's `br`
  instructions. The phi-argument produces an owned value which starts a new
  lifetime. The combination of all those lifetimes is a _forward-extended_
  lifetime:

```
      cond_br %cond, bb1, bb2
    bb1:
      %1 = copy_value %0       -+ lifetime of %1     -+ forward-extended
      br bb3(%1)               -/                     |   lifetime
    bb2:                                              |
      %3 = load [copy] %addr   -+ lifetime of %3      |
      br bb3(%2)               -/                     |
    bb3(%phi : @owned $C):     -+ lifetime of %phi    |
      return %phi              -/                    -+
```

- `@reborrow`: The incoming values must be borrow-introducers and their borrow
  scopes end at the predecessor's `br` instructions. The phi-argument produces
  a guaranteed value which is borrow-introducing as well. The overall
  forward-extended borrow scope is a combination of all the individual borrow
  scopes.  A _reborrow_ argument must be directly forwarded to a
  [`borrowed-from`](Instructions.md#borrowed-from) instruction in the same
  basic block. The `borrowed-from` instruction specifies the enclosing values
  of the argument value.

```
      %1 = copy_addr %0
      cond_br %cond, bb1, bb2
    bb1:
      %2 = begin_borrow %1     -+ lifetime of %2     -+ forward-extended
      br bb3(%x, %2)           -/                     |   borrow scope
    bb2:                                              |
      %3 = load [copy] %addr                          |
      %4 = begin_borrow %1     -+ lifetime of %4      |
      br bb3(%3, %4)           -/                     |
    bb3(%p1 : @owned $C, %p2 : @reborrow $C): -+------|-- lifetime of %p2
      %6 = borrowed %p2 from (%p1, %1)         |      |
      end_borrow %6                           -+     -+
```

- `@guaranteed`: The incoming values can be either borrow introducers or
  forwarded guaranteed values. The phi argument does _not_ produce a new
  lifetime. Instead the argument can be viewed as interior use of another
  enclosing borrow scope. Such guaranteed phi-arguments are called "forwarded
  guaranteed phis".  
  Like reborrow arguments, the argument's enclosing values (= its borrow
  introducers) must be specified with a
  [`borrowed-from`](Instructions.md#borrowed-from) instruction.

```
      cond_br %cond, bb1, bb2
    bb1:
      %2 = begin_borrow %0                                  -+ forward-extended
      %3 = struct_extract %2, #S.a     // interior use       |   borrow scope
      br bb3(%2, %3)                                         |
    bb2:                                                     |
      %4 = begin_borrow %0                                   |
      %5 = struct_extract %2, #S.b     // interior use       |
      br bb3(%4, %5)                                         |
    bb3(%p1 : @reborrow $C, %p2 : @guaranteed $D):           |
      %6 = borrowed %p1 from (%0)                            |
      %7 = borrowed %p2 from (%p1)     // interior use       |
      %8 = ref_element_addr %7, #D.f   // interior use       |
      end_borrow %6                                         -+
```

### Lexical Lifetimes and Deinit Barriers

The optimizer is free to shrink lifetimes (e.g. by hoisting `destroy_value`
instructions) as long as all interior uses and inner borrow scopes stay
within the lifetime.

A forward-extended lifetimes can be marked as _lexical_ by either a `move_value
[lexical]` or `begin_borrow [lexical]` instruction. For such lifetimes further
restrictions apply. A destroy of such a value must not be hoisted above an
instruction which is or can be a _deinit barrier_.

Only the initial producer of a forward-extended lifetime needs to be marked as
_lexical_ in order to specify this property for all contributing lifetimes.

For details see [Variable Lifetimes](Ownership.md#variable-lifetimes) in the
Ownership document.

# Debug Information

Each instruction may have a debug location and a SIL scope reference at
the end.

```
sil-scope-ref ::= 'scope' [0-9]+
sil-scope ::= 'sil_scope' [0-9]+ '{'
                 sil-loc
                 'parent' scope-parent
                 ('inlined_at' sil-scope-ref)?
              '}'
scope-parent ::= sil-function-name ':' sil-type
scope-parent ::= sil-scope-ref
sil-loc ::= 'loc' string-literal ':' [0-9]+ ':' [0-9]+
```

Debug locations consist of a filename, a line number, and a
column number. If the debug location is omitted, it defaults to the
location in the SIL source file. SIL scopes describe the position inside
the lexical scope structure that the Swift expression a SIL instruction
was generated from had originally. SIL scopes also hold inlining
information.

# Declaration References

Some SIL instructions need to reference Swift declarations directly.
These references are introduced with the `#` sigil followed by the fully
qualified name of the Swift declaration. 

```
sil-decl-ref ::= '#' sil-identifier ('.' sil-identifier)* sil-decl-subref?
sil-decl-subref ::= '!' sil-decl-subref-part ('.' sil-decl-lang)? ('.' sil-decl-autodiff)?
sil-decl-subref ::= '!' sil-decl-lang
sil-decl-subref ::= '!' sil-decl-autodiff
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
sil-decl-lang ::= 'foreign'
sil-decl-autodiff ::= sil-decl-autodiff-kind '.' sil-decl-autodiff-indices
sil-decl-autodiff-kind ::= 'jvp'
sil-decl-autodiff-kind ::= 'vjp'
sil-decl-autodiff-indices ::= [SU]+
```

Some Swift declarations are
decomposed into multiple entities at the SIL level. These are
distinguished by following the qualified name with `!` and one or more
`.`-separated component entity discriminators:

-   `getter`: the getter function for a `var` declaration
-   `setter`: the setter function for a `var` declaration
-   `allocator`: a `struct` or `enum` constructor, or a `class`'s
    *allocating constructor*
-   `initializer`: a `class`'s *initializing constructor*
-   `enumelt`: a member of a `enum` type.
-   `destroyer`: a class's destroying destructor
-   `deallocator`: a class's deallocating destructor
-   `globalaccessor`: the addressor function for a global variable
-   `ivardestroyer`: a class's ivar destroyer
-   `ivarinitializer`: a class's ivar initializer
-   `defaultarg.`*n*: the default argument-generating function for the
    *n*-th argument of a Swift `func`
-   `foreign`: a specific entry point for C/Objective-C interoperability

# Linkage

A linkage specifier controls the situations in which two objects in
different SIL modules are *linked*, i.e. treated as the same object.

```
sil-linkage ::= 'public'
sil-linkage ::= 'non_abi'
sil-linkage ::= 'package'
sil-linkage ::= 'package_non_abi'
sil-linkage ::= 'hidden'
sil-linkage ::= 'shared'
sil-linkage ::= 'private'
sil-linkage ::= 'public_external'
sil-linkage ::= 'package_external'
sil-linkage ::= 'hidden_external'
```

A linkage is *external* if it ends with the suffix `external`. An object
must be a definition if its linkage is not external.

All functions, global variables, and witness tables have linkage. The
default linkage of a definition is `public`. The default linkage of a
declaration is `public_external`.

On a global variable, an external linkage is what indicates that the
variable is not a definition. A variable lacking an explicit linkage
specifier is presumed a definition (and thus gets the default linkage
for definitions, `public`.)

## Definition of the *linked* relation

Two objects are linked if they have the same name and are mutually
visible:

-   An object with `public` or `public_external` linkage is always
    visible.
-   An object with `package` or `package_external` linkage is visible
    only to objects in the same Swift package.
-   An object with `hidden`, `hidden_external`, or `shared` linkage is
    visible only to objects in the same Swift module.
-   An object with `private` linkage is visible only to objects in the
    same SIL module.

Note that the *linked* relationship is an equivalence relation: it is
reflexive, symmetric, and transitive.

## Requirements on linked objects

If two objects are linked, they must have the same type.

If two objects are linked, they must have the same linkage, except:

-   A `public` object may be linked to a `public_external` object.
-   A `package` object may be linked to a `package_external` object.
-   A `hidden` object may be linked to a `hidden_external` object.

If two objects are linked, at most one may be a definition, unless:

-   both objects have `shared` linkage or
-   at least one of the objects has an external linkage.

If two objects are linked, and both are definitions, then the
definitions must be semantically equivalent. This equivalence may exist
only on the level of user-visible semantics of well-defined code; it
should not be taken to guarantee that the linked definitions are exactly
operationally equivalent. For example, one definition of a function
might copy a value out of an address parameter, while another may have
had an analysis applied to prove that said value is not needed.

## Non-ABI linkage

The `non_abi` and `package_non_abi` linkages are special linkages used for
definitions which only exist in serialized SIL, and do not define
visible symbols in the object file.

A definition with `non_abi` or `package_non_abi`  linkage behaves like it has
`shared` linkage, except that it must be serialized in the
SIL module even if not referenced from anywhere else in the module. For
example, this means it is considered a root for dead function
elimination.

When a `non_abi` definition is deserialized, it will have `shared` linkage.

There is no `non_abi_external` linkage. Instead,
referencing a `public_non_abi` or `package_non_abi` declaration is done with
- `hidden_external` if it is in a different source file but in the same module,
- `public_external` if it is in a different module.

# VTables

The potential destinations for [class_method](Instructions.md#class_method) and
[super_method](Instructions.md#super_method) are tracked in `sil_vtable` declarations
for every class type. The declaration contains a mapping from every
method of the class (including those inherited from its base class) to
the SIL function that implements the method for that class:

```
decl ::= sil-vtable
sil-vtable ::= 'sil_vtable' identifier '{' sil-vtable-entry* '}'
sil-vtable ::= 'sil_vtable' sil-type '{' sil-vtable-entry* '}'

sil-vtable-entry ::= sil-decl-ref ':' sil-linkage? sil-function-name
```

SIL represents dynamic dispatch for class methods using the
[class_method](Instructions.md#class_method),
[super_method](Instructions.md#super_method),
[objc_method](Instructions.md#objc_method),
and [objc_super_method](Instructions.md#objc_super_method) instructions.

```
class A {
  func foo()
  func bar()
  func bas()
}

sil @A_foo : $@convention(thin) (@owned A) -> ()
sil @A_bar : $@convention(thin) (@owned A) -> ()
sil @A_bas : $@convention(thin) (@owned A) -> ()

sil_vtable A {
  #A.foo: @A_foo
  #A.bar: @A_bar
  #A.bas: @A_bas
}

class B : A {
  func bar()
}

sil @B_bar : $@convention(thin) (@owned B) -> ()

sil_vtable B {
  #A.foo: @A_foo
  #A.bar: @B_bar
  #A.bas: @A_bas
}

class C : B {
  func bas()
}

sil @C_bas : $@convention(thin) (@owned C) -> ()

sil_vtable C {
  #A.foo: @A_foo
  #A.bar: @B_bar
  #A.bas: @C_bas
}
```

Note that the declaration reference in the vtable is to the
least-derived method visible through that class (in the example above,
`B`'s vtable references `A.bar` and not `B.bar`, and `C`'s vtable
references `A.bas` and not `C.bas`). The Swift AST maintains override
relationships between declarations that can be used to look up
overridden methods in the SIL vtable for a derived class (such as
`C.bas` in `C`'s vtable).

In case the SIL function is a thunk, the function name is preceded with
the linkage of the original implementing function.

If the vtable refers to a specialized class, a SIL type specifies the
bound generic class type:

```
sil_vtable $G<Int> {
  // ...
}
```

# Witness Tables

```
decl ::= sil-witness-table
sil-witness-table ::= 'sil_witness_table' sil-linkage?
                      normal-protocol-conformance '{' sil-witness-entry* '}'
```

SIL encodes the information needed for dynamic dispatch of generic types
into witness tables. This information is used to produce runtime
dispatch tables when generating binary code. A witness table is
emitted for every declared explicit conformance. Generic types share one
generic witness table for all of their instances. Derived classes
inherit the witness tables of their base class.

```
protocol-conformance ::= normal-protocol-conformance
protocol-conformance ::= 'inherit' '(' protocol-conformance ')'
protocol-conformance ::= 'specialize' '<' substitution* '>'
                         '(' protocol-conformance ')'
protocol-conformance ::= 'dependent'
normal-protocol-conformance ::= identifier ':' identifier 'module' identifier
```

Witness tables are keyed by *protocol conformance*, which is a unique
identifier for a concrete type's conformance to a protocol.

-   A *normal protocol conformance* names a (potentially unbound
    generic) type, the protocol it conforms to, and the module in which
    the type or extension declaration that provides the conformance
    appears. These correspond 1:1 to protocol conformance declarations
    in the source code.
-   If a derived class conforms to a protocol through inheritance from
    its base class, this is represented by an *inherited protocol
    conformance*, which simply references the protocol conformance for
    the base class.
-   If an instance of a generic type conforms to a protocol, it does so
    with a *specialized conformance*, which provides the generic
    parameter bindings to the normal conformance, which should be for a
    generic type.

Witness tables are only directly associated with normal conformances.
Inherited and specialized conformances indirectly reference the witness
table of the underlying normal conformance.

```
sil-witness-entry ::= 'base_protocol' identifier ':' protocol-conformance
sil-witness-entry ::= 'method' sil-decl-ref ':' sil-function-name
sil-witness-entry ::= 'associated_type' identifier
sil-witness-entry ::= 'associated_conformance'
                      '(' identifier ':' identifier ')' ':' protocol-conformance
```

Witness tables consist of the following entries:

-   *Base protocol entries* provide references to the protocol
    conformances that satisfy the witnessed protocols' inherited
    protocols.
-   *Method entries* map a method requirement of the protocol to a SIL
    function that implements that method for the witness type. One
    method entry must exist for every required method of the witnessed
    protocol.
-   *Associated type entries* map an associated type requirement of the
    protocol to the type that satisfies that requirement for the witness
    type. Note that the witness type is a source-level Swift type and
    not a SIL type. One associated type entry must exist for every
    required associated type of the witnessed protocol.
-   *Associated type protocol entries* map a protocol requirement on an
    associated type to the protocol conformance that satisfies that
    requirement for the associated type.

# Default Witness Tables

```
decl ::= sil-default-witness-table
sil-default-witness-table ::= 'sil_default_witness_table'
                              identifier minimum-witness-table-size
                              '{' sil-default-witness-entry* '}'
minimum-witness-table-size ::= integer
```

SIL encodes requirements with resilient default implementations in a
default witness table. We say a requirement has a resilient default
implementation if the following conditions hold:

-   The requirement has a default implementation
-   The requirement is either the last requirement in the protocol, or
    all subsequent requirements also have resilient default
    implementations

The set of requirements with resilient default implementations is stored
in protocol metadata.

The minimum witness table size is the size of the witness table, in
words, not including any requirements with resilient default
implementations.

Any conforming witness table must have a size between the minimum size,
and the maximum size, which is equal to the minimum size plus the number
of default requirements.

At load time, if the runtime encounters a witness table with fewer than
the maximum number of witnesses, the witness table is copied, with
default witnesses copied in. This ensures that callers can always expect
to find the correct number of requirements in each witness table, and
new requirements can be added by the framework author, without breaking
client code, as long as the new requirements have resilient default
implementations.

Default witness tables are keyed by the protocol itself. Only protocols
with public visibility need a default witness table; private and
internal protocols are never seen outside the module, therefore there
are no resilience issues with adding new requirements.

```
sil-default-witness-entry ::= 'method' sil-decl-ref ':' sil-function-name
```

Default witness tables currently contain only one type of entry:

-   *Method entries* map a method requirement of the protocol to a SIL
    function that implements that method in a manner suitable for all
    witness types.

# Global Variables

The SIL representation of a global variable:

```
decl ::= sil-global-variable
static-initializer ::= '=' '{' sil-instruction-def* '}'
sil-global-variable ::= 'sil_global' sil-linkage identifier ':' sil-type
                           (static-initializer)?
```

Global variable access is performed by the `alloc_global`, `global_addr`
and `global_value` instructions.

A global can have a static initializer if its initial value can be
composed of literals. The static initializer is represented as a list of
literal and aggregate instructions where the last instruction is the
top-level value of the static initializer:

```
sil_global hidden @$S4test3varSiv : $Int {
  %0 = integer_literal $Builtin.Int64, 27
  %initval = struct $Int (%0 : $Builtin.Int64)
}
```

If a global variable does not have a static initializer, the `alloc_global`
instruction must be performed prior an access to initialize the storage.
Once a global's storage has been initialized, `global_addr` is used to
project the value.

If the last instruction in the static initializer is an `object`
instruction the global variable is a statically initialized object. In
this case the variable cannot be used as l-value, i.e. the reference to
the object cannot be modified. As a consequence the variable cannot be
accessed with `global_addr` but only with `global_value`.

# Differentiability Witnesses

```
decl ::= sil-differentiability-witness
sil-differentiability-witness ::=
    'sil_differentiability_witness'
    sil-linkage?
    '[' differentiability-kind ']'
    '[' 'parameters' sil-differentiability-witness-function-index-list ']'
    '[' 'results' sil-differentiability-witness-function-index-list ']'
    generic-parameter-clause?
    sil-function-name ':' sil-type
    sil-differentiability-witness-body?

differentiability-kind ::= 'forward' | 'reverse' | 'normal' | 'linear'

sil-differentiability-witness-body ::=
    '{' sil-differentiability-witness-entry?
        sil-differentiability-witness-entry? '}'

sil-differentiability-witness-entry ::=
    sil-differentiability-witness-entry-kind ':'
    sil-entry-name ':' sil-type

sil-differentiability-witness-entry-kind ::= 'jvp' | 'vjp'
```

SIL encodes function differentiability via differentiability witnesses.

Differentiability witnesses map a "key" (including an "original" SIL
function) to derivative SIL functions.

Differentiability witnesses are keyed by the following:

-   An "original" SIL function name.
-   Differentiability parameter indices.
-   Differentiability result indices.
-   A generic parameter clause, representing differentiability generic
    requirements.

Differentiability witnesses may have a body, specifying derivative
functions for the key. Verification checks that derivative functions
have the expected type based on the key.

```
sil_differentiability_witness hidden [normal] [parameters 0] [results 0] <T where T : Differentiable> @id : $@convention(thin) (T) -> T {
  jvp: @id_jvp : $@convention(thin) (T) -> (T, @owned @callee_guaranteed (T.TangentVector) -> T.TangentVector)
  vjp: @id_vjp : $@convention(thin) (T) -> (T, @owned @callee_guaranteed (T.TangentVector) -> T.TangentVector)
}
```

During SILGen, differentiability witnesses are emitted for the
following:

-   `@differentiable` declaration attributes.
-   `@derivative` declaration attributes. Registered
    derivative functions become differentiability witness entries.

The SIL differentiation transform canonicalizes differentiability
witnesses, filling in missing entries.

Differentiability witness entries are accessed via the
`differentiability_witness_function` instruction.

# Copy-on-Write Representation

Copy-on-Write (COW) data structures are implemented by a reference to an
object which is copied on mutation in case it's not uniquely
referenced.

A COW mutation sequence in SIL typically looks like:

```
  (%uniq, %buffer) = begin_cow_mutation %immutable_buffer : $BufferClass
  cond_br %uniq, bb_uniq, bb_not_unique
bb_uniq:
  br bb_mutate(%buffer : $BufferClass)
bb_not_unique:
  %copied_buffer = apply %copy_buffer_function(%buffer) : ...
  br bb_mutate(%copied_buffer : $BufferClass)
bb_mutate(%mutable_buffer : $BufferClass):
  %field = ref_element_addr %mutable_buffer : $BufferClass, #BufferClass.Field
  store %value to %field : $ValueType
  %new_immutable_buffer = end_cow_mutation %buffer : $BufferClass
```

Loading from a COW data structure looks like:

```
  %field1 = ref_element_addr [immutable] %immutable_buffer : $BufferClass, #BufferClass.Field
  %value1 = load %field1 : $*FieldType
...
  %field2 = ref_element_addr [immutable] %immutable_buffer : $BufferClass, #BufferClass.Field
  %value2 = load %field2 : $*FieldType
```

The `immutable` attribute means that loading values from
`ref_element_addr` and `ref_tail_addr` instructions, which have the
*same* operand, are equivalent. In other words, it's guaranteed that a
buffer's properties are not mutated between two
`ref_element/tail_addr [immutable]` as long as they have the same buffer
reference as operand. This is even true if e.g. the buffer 'escapes'
to an unknown function.

In the example above, `%value2` is equal to `%value1` because the
operand of both `ref_element_addr` instructions is the same
`%immutable_buffer`. Conceptually, the content of a COW buffer object
can be seen as part of the same *static* (immutable) SSA value as the
buffer reference.

The lifetime of a COW value is strictly separated into *mutable* and
*immutable* regions by `begin_cow_mutation` and `end_cow_mutation`
instructions:

```
  %b1 = alloc_ref $BufferClass
  // The buffer %b1 is mutable
  %b2 = end_cow_mutation %b1 : $BufferClass
  // The buffer %b2 is immutable
  (%u1, %b3) = begin_cow_mutation %b1 : $BufferClass
  // The buffer %b3 is mutable
  %b4 = end_cow_mutation %b3 : $BufferClass
  // The buffer %b4 is immutable
...
```

Both, `begin_cow_mutation` and `end_cow_mutation`, consume their operand
and return the new buffer as an *owned* value. The `begin_cow_mutation`
will compile down to a uniqueness check and `end_cow_mutation` will
compile to a no-op.

Although the physical pointer value of the returned buffer reference is
the same as the operand, it's important to generate a *new* buffer
reference in SIL. It prevents the optimizer from moving buffer accesses
from a *mutable* into a *immutable* region and vice versa.

Because the buffer *content* is conceptually part of the buffer
*reference* SSA value, there must be a new buffer reference every time
the buffer content is mutated.

To illustrate this, let's look at an example, where a COW value is
mutated in a loop. As with a scalar SSA value, also mutating a COW
buffer will enforce a phi-argument in the loop header block (for
simplicity the code for copying a non-unique buffer is not shown):

```
header_block(%b_phi : $BufferClass):
  (%u, %b_mutate) = begin_cow_mutation %b_phi : $BufferClass
  // Store something to %b_mutate
  %b_immutable = end_cow_mutation %b_mutate : $BufferClass
  cond_br %loop_cond, exit_block, backedge_block
backedge_block:
  br header_block(b_immutable : $BufferClass)
exit_block:
```

Two adjacent `begin_cow_mutation` and `end_cow_mutation` instructions
don't need to be in the same function.

# Stack discipline

Certain instructions in the SIL instruction set are identified as *stack
allocation instructions* or *stack deallocation instructions*. These
instructions jointly participate in a set of rules called the *stack
allocation discipline*, designed to allow SIL functions to easily and
safely dynamically allocate and deallocate memory in a scoped fashion on
the stack.

All stack deallocation instructions have an operand which identifies
their *paired* stack allocation instruction. This operand must always be
exactly the result of a stack allocation instruction, with no
intervening conversions, basic block arguments, or other abstractions. A
single stack allocation instruction may be paired with any number of
stack deallocation instructions. It can even be paired with no
instructions at all; by the rules below, this can only happen in
non-terminating functions.

-   At any point in a SIL function, there is an ordered list of stack
    allocation instructions called the *active allocations list*.

-   The active allocations list is defined to be empty at the initial
    point of the entry block of the function.

-   The active allocations list is required to be the same at the
    initial point of any successor block as it is at the final point of
    any predecessor block. Note that this also requires all
    predecessors/successors of a given block to have the same
    final/initial active allocations lists.

    In other words, the set of active stack allocations must be the same
    at a given place in the function no matter how it was reached.

-   The active allocations list for the point following a stack
    allocation instruction is defined to be the result of adding that
    instruction to the end of the active allocations list for the point
    preceding the instruction.

-   The active allocations list for the point following a stack
    deallocation instruction is defined to be the result of removing the
    instruction from the end of the active allocations list for the
    point preceding the instruction. The active allocations list for the
    preceding point is required to be non-empty, and the last
    instruction in it must be paired with the deallocation instruction.

    In other words, all stack allocations must be deallocated in
    last-in, first-out order, aka stack order.

-   The active allocations list for the point following any other
    instruction is defined to be the same as the active allocations list
    for the point preceding the instruction.

-   The active allocations list is required to be empty prior to
    `return` or `throw` instructions.

    In other words, all stack allocations must be deallocated prior to
    exiting the function.

Note that these rules implicitly prevent an allocation instruction from
still being active when it is reached.

The control-flow rule forbids certain patterns that would theoretically
be useful, such as conditionally performing an allocation around an
operation. SIL generally makes this sort of pattern somewhat difficult
to use, however, as it is illegal to locally abstract over addresses,
and therefore a conditional allocation cannot be used in the
intermediate operation anyway.

# Structural type matching for pack indices

In order to catch type errors in applying pack indices, SIL requires the
projected element types of pack-indexing operations to be *structurally
well-typed* for the given pack type and index.

First, the projected element type must match the direct-ness of the
indexed pack type: if the pack is indirect, the project element type
must be an address type, and otherwise it must be an object type.

Second, the pack index must be a *pack indexing instruction* (one of
`scalar_pack_index`, `pack_pack_index`, or `dynamic_pack_index`), and it
must index into a pack type with the same shape as the indexed pack
type.

Third, additional restrictions must be satisfied depending on which pack
indexing instruction the pack index is:

-   For `scalar_pack_index`, the projected element type must be the same
    type as the scalar type at the given index in the pack type. (It
    must be a scalar type because of the shape restriction above.)

-   For `pack_pack_index`, the projected element type must be
    structurally well-typed for a slice of the pack type (as specified
    by the instruction) at the pack sub-index operand.

-   For `dynamic_pack_index`, consider each opened pack element
    archetype in the projected element type that is opened by an
    `open_pack_element` instruction whose pack index operand is the same
    `dynamic_pack_index` instruction. Because the pack substitutions in
    `open_pack_element` must have the same shape as the indexed pack
    type of its pack index operand, by transitivity they must have the
    same shape as the indexed pack type of the pack-indexing operation.
    Then for each component of this shape, the corresponding element
    component (or the pattern type for a projection component) of the
    indexed pack type must equal the result of applying a substitution
    to the projected element type which replaces any opened pack element
    archetype with the corresponding element component (pattern type for
    a projection component) of the pack substitution for that archetype
    in the `open_pack_element` which introduced it.

    For example, if the indexed pack type is
    `Pack{Optional<Int>, Optional<Float>, repeat Optional<each T>}`, a
    projected element type is `$*Optional<@pack_element("1234") U>` is
    structurally well-typed for a `dynamic_pack_index` pack index if
    `1234` is the UUID of an `open_pack_element` indexed by the same
    `dynamic_pack_index` instruction and the pack substitution
    corresponding to `U` in that `open_pack_element` is
    `Pack{Int, Float, repeat each T}`.

# Memory Lifetime

This section describes lifetime rules for values in
memory. With "memory" we refer to memory which is addressed by SIL
instruction with address-type operands, like `load`, `store`,
`switch_enum_addr`, etc.

Each memory location which holds a non-trivial value is either
uninitialized or initialized. A memory location gets initialized by
storing values into it (except assignment, which expects a location to
be already initialized). A memory location gets de-initialized by
"taking" from it or destroying it, e.g. with `destroy_addr`. It is
illegal to re-initialize a memory location or to use a location after it
was de-initialized.

If a memory location holds a trivial value (e.g. an `Int`), it is not
required to de-initialize the location.

The SIL verifier checks this rule for memory locations which can be
uniquely identified, for example and `alloc_stack` or an indirect
parameter. The verifier cannot check memory locations which are
potentially aliased, e.g. a `ref_element_addr` (a stored class
property).

## Lifetime of Enums in Memory

The situation is a bit more complicated with enums, because an enum can
have both, cases with non-trivial payloads and cases with no payload or
trivial payloads.

Even if an enum itself is not trivial (because it has at least on case
with a non-trivial payload), it is not required to de-initialize such an
enum memory location on paths where it's statically provable that the
enum contains a trivial or non-payload case.

That's the case if the destroy point is jointly dominated by:

-   a `store [trivial]` to the enum memory location.

or

-   an `inject_enum_addr` to the enum memory location with a trivial or
    non-payload case.

or

-   a successor of a `switch_enum` or `switch_enum_addr` for a trivial
    or non-payload case.

# Type Based Alias Analysis

SIL supports two types of Type Based Alias Analysis (TBAA): Class TBAA
and Typed Access TBAA.

## Class TBAA

Class instances and other *heap object references* are pointers at the
implementation level, but unlike SIL addresses, they are first class
values and can be `capture`-d and aliased. Swift, however, is
memory-safe and statically typed, so aliasing of classes is constrained
by the type system as follows:

-   A `Builtin.NativeObject` may alias any native Swift heap object,
    including a Swift class instance, a box allocated by `alloc_box`, or
    a thick function's closure context. It may not alias natively
    Objective-C class instances.
-   An `AnyObject` or `Builtin.BridgeObject` may alias any class
    instance, whether Swift or Objective-C, but may not alias
    non-class-instance heap objects.
-   Two values of the same class type `$C` may alias. Two values of
    related class type `$B` and `$D`, where there is a subclass
    relationship between `$B` and `$D`, may alias. Two values of
    unrelated class types may not alias. This includes different
    instantiations of a generic class type, such as `$C<Int>` and
    `$C<Float>`, which currently may never alias.
-   Without whole-program visibility, values of archetype or protocol
    type must be assumed to potentially alias any class instance. Even
    if it is locally apparent that a class does not conform to that
    protocol, another component may introduce a conformance by an
    extension. Similarly, a generic class instance, such as `$C<T>` for
    archetype `T`, must be assumed to potentially alias concrete
    instances of the generic type, such as `$C<Int>`, because `Int` is a
    potential substitution for `T`.

A violation of the above aliasing rules only results in undefined
behavior if the aliasing references are dereferenced within Swift code.
For example, `__SwiftNativeNS[Array|Dictionary|String]` classes alias
with `NS[Array|Dictionary|String]` classes even though they are not
statically related. Since Swift never directly accesses stored
properties on the Foundation classes, this aliasing does not pose a
danger.

## Typed Access TBAA

Define a *typed access* of an address or reference as one of the
following:

-   Any instruction that performs a typed read or write operation upon
    the memory at the given location (e.x. `load`, `store`).
-   Any instruction that yields a typed offset of the pointer by
    performing a typed projection operation (e.x. `ref_element_addr`,
    `tuple_element_addr`).

With limited exceptions, it is undefined behavior to perform a typed
access to an address or reference addressed memory is not bound to the
relevant type.

This allows the optimizer to assume that two addresses cannot alias if
there does not exist a substitution of archetypes that could cause one
of the types to be the type of a subobject of the other. Additionally,
this applies to the types of the values from which the addresses were
derived via a typed projection.

Consider the following SIL:

```
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
```

The optimizer may assume that `%adr1` does not alias with `%adr2`
because the values that the addresses are derived from (`%ptr1` and
`%ptr2`) have unrelated types. However, in the following example, the
optimizer cannot assume that `%adr1` does not alias with `%adr2` because
`%adr2` is derived from a cast, and any subsequent typed operations on
the address will refer to the common `Element` type:

```
%adr1 = struct_element_addr %ptr1 : $*S1, #S.elt
%adr2 = pointer_to_address %ptr2 : $Builtin.RawPointer to $*Element
```

Exceptions to typed access TBAA rules are only allowed for blessed
alias-introducing operations. This permits limited type-punning. The
only current exception is the non-struct `pointer_to_address` variant.
The optimizer must be able to defensively determine that none of the
*roots* of an address are alias-introducing operations. An address root
is the operation that produces the address prior to applying any typed
projections, indexing, or casts. The following are valid address roots:

-   Object allocation that generates an address, such as `alloc_stack`
    and `alloc_box`.
-   Address-type function arguments. These are crucially *not*
    considered alias-introducing operations. It is illegal for the SIL
    optimizer to form a new function argument from an arbitrary
    address-type value. Doing so would require the optimizer to
    guarantee that the new argument is both has a non-alias-introducing
    address root and can be properly represented by the calling
    convention (address types do not have a fixed representation).
-   A strict cast from an untyped pointer,
    `pointer_to_address [strict]`. It is illegal for
    `pointer_to_address [strict]` to derive its address from an
    alias-introducing operation's value. A type punned address may only
    be produced from an opaque pointer via a non-strict
    `pointer_to_address` at the point of conversion.

Address-to-address casts, via `unchecked_addr_cast`, transparently
forward their source's address root, just like typed projections.

Address-type basic block arguments can be conservatively considered
aliasing-introducing operations; they are uncommon enough not to matter
and may eventually be prohibited altogether.

Although some pointer producing intrinsics exist, they do not need to be
considered alias-introducing exceptions to TBAA rules.
`Builtin.inttoptr` produces a `Builtin.RawPointer` which is not
interesting because by definition it may alias with everything.
Similarly, the LLVM builtins `Builtin.bitcast` and
`Builtin.trunc|sext|zextBitCast` cannot produce typed pointers. These
pointer values must be converted to an address via `pointer_to_address`
before typed access can occur. Whether the `pointer_to_address` is
strict determines whether aliasing may occur.

Memory may be rebound to an unrelated type. Addresses to unrelated types
may alias as long as typed access only occurs while memory is bound to
the relevant type. Consequently, the optimizer cannot outright assume
that addresses accessed as unrelated types are nonaliasing. For example,
pointer comparison cannot be eliminated simply because the two addresses
derived from those pointers are accessed as unrelated types at different
program points.

# Runtime Failure

Some operations, such as failed unconditional checked conversions or the
`cond_fail` instruction, cause a *runtime failure*, which terminates the
program. A runtime failure may be reordered freely as long as:

- it does not expose undefined behavior which is protected by the runtime
  failure; for example, it's illegal to move bounds checking past a potential
  buffer overflow.

- it only triggers in control flow paths where it would have been triggered
  originally; for example it's illegal to hoist a runtime failure out of a loop
  which may have a trip count of zero.

It is explicitly allowed to reorder runtime failures with externally visible
program behavior. For example, it's allowed to hoist a runtime failure above a
print statement.

# Undefined Behavior

Incorrect use of some operations is *undefined behavior*, such as
invalid unchecked casts involving `Builtin.RawPointer` types, or use of
compiler builtins that lower to LLVM instructions with undefined
behavior at the LLVM level. A SIL program with undefined behavior is
meaningless, much like undefined behavior in C, and has no predictable
semantics. Undefined behavior should not be triggered by valid SIL
emitted by a correct Swift program using a correct standard library, but
cannot in all cases be diagnosed or verified at the SIL level.

