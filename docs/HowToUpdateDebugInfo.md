## How to update Debug Info in the Swift Compiler

### Introduction

This document describes how debug info works at the SIL level and how to
correctly update debug info in SIL optimization passes. This document is
inspired by its LLVM analog, [How to Update Debug Info: A Guide for LLVM Pass
Authors](https://llvm.org/docs/HowToUpdateDebugInfo.html), which is recommended
reading, since all of the concepts discussed there also apply to SIL.

### Source Locations

Contrary to LLVM IR, SIL makes source locations and lexical scopes mandatory on
all instructions. SIL transformations should follow the LLVM guide for when to
merge drop and copy locations, since all the same considerations apply. Helpers
like `SILBuilderWithScope` make it easy to copy source locations when expanding
SIL instructions.

### Variables, Variable Locations

Source variables are represented by `debug_value` instructions, and may also be
described in variable-carrying instructions (`alloc_stack`, `alloc_box`). There
is no semantic difference between describing a variable in an allocation
instruction directly or describing it in an `debug_value` following the
allocation instruction. Variables are uniquely identified via their lexical
scope, which also includes inline information, and their name and binding kind.

Each `debug_value` (and variable-carrying instruction) defines an update point
for the location of (part of) that source variable. A variable location is an
SSA value or constant, modified by a debug expression that can transform that
value, yielding the value of that variable. The debug expressions get lowered
into LLVM [DIExpressions](https://llvm.org/docs/LangRef.html#diexpression) which
get lowered into [DWARF](https://dwarfstd.org) expressions. Optimizations like
SROA may split a source variable into multiple smaller fragments. An
`op_fragment` is used to denote a location of a partial variable. Each variable
(fragment) location is valid until the end of the current basic block, or until
another `debug_value` describes another location for a variable fragment for the
same unique variable that overlaps with that (fragment of the) variable.
Variables may be undefined, in which case the SSA value is `undef`.

### Rules of thumb
- Optimization passes may never drop a variable entirely. If a variable is
  entirely optimized away, an `undef` debug value should still be kept.
- A `debug_value` must always describe a correct value for that source variable
  at that source location. If a value is only correct on some paths through that
  instruction, it must be replaced with `undef`. Debug info never speculates.
- When a SIL instruction referenced by a `debug_value` is (really, any
  instruction) deleted, call salvageDebugInfo(). It will try to capture the
  effect of the deleted instruction in a debug expression, so the location can
  be preserved.
