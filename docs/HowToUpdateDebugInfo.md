# How to update Debug Info in the Swift Compiler

## Introduction

This document describes how debug info works at the SIL level and how to
correctly update debug info in SIL optimization passes. This document is
inspired by its LLVM analog, [How to Update Debug Info: A Guide for LLVM Pass
Authors](https://llvm.org/docs/HowToUpdateDebugInfo.html), which is recommended
reading, since all of the concepts discussed there also apply to SIL.

## Source Locations

Contrary to LLVM IR, SIL makes source locations and lexical scopes mandatory on
all instructions. SIL transformations should follow the LLVM guide for when to
merge drop and copy locations, since all the same considerations apply. Helpers
like `SILBuilderWithScope` make it easy to copy source locations when expanding
SIL instructions.

> [!Warning]
> Don't use `SILBuilderWithScope` when replacing a single instruction of type
> `AllocStackInst` or `DebugValueInst`. These meta instructions are skipped,
> so the wrong scope will be inferred.

## Variables

Each `debug_value` (and variable-carrying instruction) defines an update point
for the location of (part of) that source variable. A variable location is an
SSA value, modified by a debug expression that can transform that value,
yielding the value of that variable. Optimizations like SROA may split a source
variable into multiple smaller fragments, other optimizations such as Mem2Reg
may split a debug value describing an address into multiple debug values
describing different SSA values. Each variable (fragment) location is valid
until the end of the current basic block, or until another `debug_value`
describes another location for a variable fragment for the same unique variable
that overlaps with that (fragment of the) variable.

### Debug variable-carrying instructions

Source variables are represented by `debug_value` instructions, and may also be
described in debug variable-carrying instructions (`alloc_stack`, `alloc_box`).
There is no semantic difference between describing a variable in an allocation
instruction directly or describing it in an `debug_value` following the
allocation instruction.

This is equivalent, and should be optimized similarly:
```
%0 = alloc_stack $T, var, name "value", loc "a.swift":4:2, scope 1
// equivalent to:
%0 = alloc_stack $T, loc "a.swift":4:2, scope 1
debug_value %0 : $*T, var, name "value", expr op_deref, loc "a.swift":4:2, scope 1
```

> [!Note]
> In the future, we may want to remove the debug variable from the `alloc_stack`
> to only use the second form, in order to simplify SIL. Additionally, we could
> then move the `debug_value` instruction to the point where the variable is
> initialized to avoid showing ununitialized memory in the debugger. This would
> be a change in SILGen, which should not affect the optimizer.

For now, the `DebugVarCarryingInst` type can be used to handle both cases.

### Variable identity, location and scope

Variables are uniquely identified via their debug scope, their location, and
their name.

The debug scope, is the range in which the variable is declared and available.
More information about debug scopes is available on
[the Swift blog](https://www.swift.org/blog/whats-new-swift-debugging-5.9/#fine-grained-scope-information)
For arguments, this will be the function's scope, otherwise, this will be a
subscope within a function. When a function is inlined, a new scope is created,
including information about the inlined function, and in which function it was
inlined (inlined_at).

The location of the variable is the source location where the variable was
declared.

If the location and scope of a debug variable isn't set, it will use the scope
and location of the instruction, which is correct in most cases. However, if a
`debug_value` describes a modification of a variable, the instruction should
have the location of the update point, and the variable must keep the location
of the variable declaration:

```
%0 = integer_literal $Int, 2
debug_value %0 : $Int, var, name "a", loc "a.swift":2:5, scope 2
%2 = integer_literal $Int, 3
debug_value %2 : $Int, var, (name "a", loc "a.swift":2:5, scope 2), loc "a.swift":3:3, scope 2
```
For this code:
```swift
var a = 2
a = 3
```

### Variable types

By default the type of the variable will be the object type of the SSA value.
If this is not the correct type, a type must be attached to the debug variable
to override it.

Example:

```
debug_value %0 : $*T, let, name "address", type $UnsafeRawPointer
```

The variable will usually have an associated expression yielding the correct
type.

> [!Note]
> As there are no pointers in Swift, the type should never be an address type.

### Variable expressions

A variable can have an associated expression if the value needs computation.
This can be for dereferencing a pointer, arithmetic, or for splitting structs.
An expression is a sequence of operations to be executed left to right. Debug
expressions get lowered into LLVM
[DIExpressions](https://llvm.org/docs/LangRef.html#diexpression) which get
lowered into [DWARF](https://dwarfstd.org) expressions.

#### Address types and op_deref

A variable's expression may include an `op_deref`, usually at the beginning, in
which case the SSA value is a pointer that must be dereferenced to access the
value of the variable.

In this example, the value returned by the `alloc_stack` is an address that must
be dereferenced.
```
%0 = alloc_stack $T
debug_value %0 : $*T, var, name "value", expr op_deref
```

SILGen should always use `SILBuilder::emitDebugDescription` to create debug
values, which will automatically add an op_deref depending on the type of the
SSA value. As there are no pointers in Swift, this will always do the right
thing. In SIL passes, use `SILBuilder::createDebugValue` to create debug values,
or `SILBuilder::createDebugValueAddr` to add an op_deref.

> [!Warning]
> At the optimizer level, Swift `Unsafe*Pointer` types can be simplified
> to address types. As such, a `debug_value` with an address type without an
> `op_deref` can be valid. SIL passes must not assume that `op_deref` and
> address types correlate.

Even if `op_deref` is usually at the beginning, it doesn't have to be:
```
debug_value %0 : $*UInt8, let, name "hello", expr op_constu:3:op_plus:op_deref
```
This will add `3` to the pointer contained in `%0`, then dereference the result.

#### Fragments

If a variable is partially updated, a fragment can be used to specify that this
update refers to an element of an aggregate type.

> [!Tip]
> When using fragments, always specify the type of the variable, as it will be
> different from the SSA value.

When SROA is splitting a struct or tuple, it will also split the debug values,
and add a fragment to specify which field is being updated.

```
struct Pair { var a, b: Int }

alloc_stack $Pair, var, name "pair"
// -->
alloc_stack $Int, var, name "pair", type $Pair, expr op_fragment:#Pair.a
alloc_stack $Int, var, name "pair", type $Pair, expr op_fragment:#Pair.b
// -->
alloc_stack $Builtin.Int64, var, name "pair", type $Pair, expr op_fragment:#Pair.a:op_fragment:#Int._value
alloc_stack $Builtin.Int64, var, name "pair", type $Pair, expr op_fragment:#Pair.b:op_fragment:#Int._value
```

Here, Pair is a struct containing two Ints, so each `alloc_stack` will receive a
fragment with the field it is describing. Int, in Swift, is itself a struct
containing one Builtin.Int64 (on 64 bits systems), so it can itself be SROA'ed.
Fragments can be chained to describe this.

Tuple fragments use a different syntax, but work similarly:

```
alloc_stack $(Int, Int), var, name "pair"
// -->
alloc_stack $Int, var, name "pair", type $(Int, Int), expr op_tuple_fragment:$(Int, Int):0
alloc_stack $Int, var, name "pair", type $(Int, Int), expr op_tuple_fragment:$(Int, Int):1
// -->
alloc_stack $Builtin.Int64, var, name "pair", type $(Int, Int), expr op_tuple_fragment:$(Int, Int):0:op_fragment:#Int._value
alloc_stack $Builtin.Int64, var, name "pair", type $(Int, Int), expr op_tuple_fragment:$(Int, Int):1:op_fragment:#Int._value
```

Tuple fragments and struct fragments can be mixed freely, however, they must all
be at the end of the expression. That is because the fragment operator can be
seen as returning a struct containing a single element, with the rest undefined,
and, except fragments, no debug expression operator take a struct as input.

> [!Note]
> When multiple fragments are present, they are evaluated in the reverse way —
> from the field within the variable first, to the SSA's type at the end

#### Arithmetic

An expression can add or subtract a constant offset to a value. To do so, an
`op_constu` or `op_consts` can be used to push a constant integer to the stack,
respectively unsigned and signed. Then, the `op_plus` and `op_minus` operators
can be used to sum or subtract the two values on the stack.

```
debug_value %0 : $Builtin.Int64, var, name "previous", type $Int, expr op_consts:1:op_minus:op_fragment:#Int._value
debug_value %0 : $Builtin.Int64, var, name "next", type $Int, expr op_consts:1:op_plus:op_fragment:#Int._value
```

> [!Caution]
> This currently doesn't work if a fragment is present.

#### Constants

If a `debug_value` is describing a constant, such as in `let x = 1`, and the
value is optimized out, we can keep it, using a constant expression, and no SSA
value.

```
debug_value undef : $Int, let, name "x", expr op_consts:1:op_fragment:#Int._value
```

### Undef variables

If the value of the variable cannot be recovered as the value is entirely
optimized away, an undef debug value should still be kept:

```
debug_value undef : $Int, let, name "x"
```

Additionally, if a previous `debug_value` exists for the variable, a debug value
of undef invalidates the previous value, in case the value of the variable isn't
known anymore:

```
debug_value %0 : $Int, var, name "x" // var x = a
...
debug_value undef : $Int, var, name "x" // x = <optimized out>
```

Combined with fragments, some parts of the variable can be undefined and some
not:

```
... // pair = ?
debug_value %0 : $Int, var, name "pair", type $Pair, expr op_fragment:#Pair.a // pair.a = x
debug_value %0 : $Int, var, name "pair", type $Pair, expr op_fragment:#Pair.b // pair.b = x
... // pair = (x, x)
debug_value undef : $Pair, var, name "pair", expr op_fragment:#Pair.a // pair.a = <optimized out>
... // pair = (?, x)
debug_value undef : $Pair, var, name "pair" // pair = <optimized out>
... // pair = ?
debug_value %1 : $Int, var, name "pair", type $Pair, expr op_fragment:#Pair.a // pair.a = y
... // pair = (y, ?)
```

## Rules of thumb

### Correctness
A `debug_value` must always describe a correct value for that source variable
at that source location. If a value is only correct on some paths through that
instruction, it must be replaced with `undef`. Debug info never speculates.

### Don't drop debug info

Optimization passes may never drop a variable entirely. If a variable is
entirely optimized away, an `undef` debug value should still be kept. The only
exception is when the variable is in an unreachable function or scope, where it
can be removed with the rest of the instructions.

### Instruction Deletion

When a SIL instruction is deleted, call `salvageDebugInfo`. It will try to
capture the effect of the deleted instruction in a debug expression, so the
location can be preserved.

Alternatively, you can use an `InstructionDeleter`, which will automatically
call `salvageDebugInfo`.

If the debug info cannot be salvaged by `salvageDebugInfo`, and the pass has a
special knowledge of the value, the pass can directly replace the debug value
with the known value.

If an instruction is being replaced by another, use `replaceAllUsesWith`. It
will also update debug values to use the new instruction.

> [!Tip]
> To detect when a pass drops a variable, you can use the
> `-Xllvm -sil-stats-lost-variables` to print when a variable is lost by a pass.
> More information about this option is available in
> [Optimizer Counter Analysis](OptimizerCountersAnalysis.md)
