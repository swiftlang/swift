# Debug Basic Blocks in SIL

## Overview

A **debug basic block** is a standalone `SILBasicBlock` attached to a `debug_value` instruction via the `transform` keyword. It contains SIL instructions that describe how to reconstruct a source variable's value from the SSA values that are still alive at that program point. These instructions are not part of the normal program, and are invisible to the optimizer, they are only used to produce DWARF debug info.

## Structure

```
struct Point {
  var x: Int
  var y: Int
}

func hello(y: Int) {
  // pt is optimized out
  let pt = Point(x: 0, y: y + 3)
  // (...)
}
```

In this program, if `pt` is optimized out, we should still have all the information needed to reconstruct `pt` in the debugger. A debug basic block can express this as follows:

```
// %0: y
debug_value %0 : $Int, let, name "pt", type $Point, transform {
bb0(%0 : $Int):
  %1 = integer_literal $Builtin.Int64, 0
  %2 = struct_extract %0 : $Int, #Int._value
  %3 = integer_literal $Builtin.Int64, 3
  %4 = builtin "add_Int64"(%2, %3)
  %5 = struct $Int (%1)
  %6 = struct $Int (%4)
  %7 = struct $Point (%5, %6)
  return %7
}
```

The `debug_value` contains a SIL basic block that takes the outside value as a phi argument, processes it, and then returns the debug variable value.

### Block arguments

The debug BB has **zero or one block argument** (matching `debug_value`'s single operand). When the argument is present, it receives the tracked value at IRGen time. When there is no argument, the reconstruction is self-contained (e.g. a constant). In the future, we might add support for multiple values being passed to a debug_value through the block argument.

Every operand of every instruction inside the debug BB must be defined _within_ the debug BB, either as a block argument or as the result of a previous instruction. Instructions in a debug BB must **never** reference values defined in the enclosing function. This ensures the optimizer can safely move or delete instructions in the enclosing function without invalidating the debug basic block.

### Permitted instructions

Instructions inside a debug BB must be **side-effect free** — no calls, no stores, no memory-allocating instructions, no trapping instructions. Permitted instructions include projections (`struct_element_addr`, `tuple_element_addr`, `struct_extract`, `tuple_extract`), arithmetic (`builtin "add_Int64"`, etc.), casts, and literal instructions (`integer_literal`, `float_literal`, `string_literal`).

The terminator is always a regular `return`.

### Combining with DIExpr

Debug basic blocks coexist with `SILDebugInfoExpression`. When both are present, the `transform` block is evaluated first, then the DIExpr evaluates `op_deref` and fragments.

For example, when each field of the struct is tracked separately using fragments:

```
// %0: y
debug_value undef : $Int, let, name "pt", type $Point, expr fragment:#Point.x:#Int._value, transform {
bb0:
  %0 = integer_literal $Builtin.Int64, 0
  return %0
}
debug_value %0 : $Int, let, name "pt", type $Point, expr fragment:#Point.y:#Int._value, transform {
bb0(%0 : $Int):
  %1 = struct_extract %0 : $Int, #Int._value
  %2 = integer_literal $Builtin.Int64, 3
  %3 = builtin "add_Int64"(%1, %2)
  return %3
}
```

## Optimizer Interaction

The optimizer does not see debug BBs. `SILFunction` iterators only walk the `BlockList`, which excludes standalone debug BBs. Optimizer passes require no changes, except for debug value handling code.

### Salvage

When `swift::salvageDebugInfo` is called before deleting an instruction, it builds a debug BB to preserve the instruction's contribution to debug info.

Example: **integer_literal salvage.**
When an `integer_literal` is deleted and has debug uses, each debug use gets a new `debug_value` with a self-contained debug BB:

```
// before
%0 = integer_literal $Builtin.Int64, 42
debug_value %0, let, name "x", type $Int, expr op_fragment:#Int._value
// after
debug_value undef, let, name "x", type $Int, expr op_fragment:#Int._value, transform {
bb0:
  %0 = integer_literal $Builtin.Int64, 42
  return %0 : $Builtin.Int64
}
```

The instruction looks the same after the salvage, it is just "copied" into the debug basic block.


### IRGen

IRGen handles debug basic blocks by generating the basic block content inline, where the `debug_value` is. Right after the debug record is created, the instructions generated as part of the debug basic block are immediately salvaged using `llvm::salvageDebugInfo`, which moves the effects of the instructions to the DWARF expression of the debug record. All instructions are then erased.

This makes sure that the LLVM pipeline never sees those instructions, while we can still leverage the existing `llvm::salvageDebugInfo`, which supports a lot of instructions. This guarantees zero impact on code generation and optimization.


## Testing

For existing SILOptimizer tests that ensure instructions are deleted, the `-sil-print-transform-blocks=false` flag can be used to suppress printing the content of debug basic blocks. That way, those instructions will not match the CHECK patterns of those tests.

