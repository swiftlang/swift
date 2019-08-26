# SIL Programmers' Manual

This document provides information for developers working on the
implementation of SIL. The formal specification of the Swift
Intermediate _Language_ is in [SIL.rst](SIL.rst). This is a guide to the internal
implementation. Source comments normally provide this level of
information as much as possible. However, some features of the
implementation are spread out across the source base. This
documentation is meant to offer a central point for approaching the
source base with links to the code for more detailed comments.

## SILType

TBD: Define the different levels of types. Explain type lowering with
examples.

## SILInstructionResults

TBD: Explain how the various types fit together with pointers to the
source: SILValue, SILInstruction, SingleValueInstruction,
MultipleValueInstructionResult. And why it was done this way.

## `SILFunction` and `apply` arguments.

Throughout the compiler, integer indices are used to identify argument
positions in several different contexts:

- A `SILFunctionType` has a tuple of parameters.

- The SIL function definition has a list of `SILFunctionArgument`.
  This is the callee-side argument list. It includes indirect results.

- `apply`, `try_apply`, and `begin_apply` have "applied arguments":
  the subset of instruction operands representing the callee's
  `SILFunctionArgument` list.

- `partial_apply` has "applied arguments": the subset of instruction
  operands representing closure captures. Closure captures in turn map
  to a subset of the callee's `SILFunctionArgument` list.

- In the above three contexts, `SILFunctionArgument`, `apply`, and
  `partial_apply`, the argument indices also depend on the SIL stage:
  Canonical vs. Lowered.

Consider the example:

```
func example<T>(i: Int, t: T) -> (Int, T) {
  let foo = { return ($0, t) }
  return foo(i)
}
```

The closure `foo` has the indices as numbered below in each
context, ignoring the calling convention and boxing/unboxing of
captures for brevity:

The closure's `SILFunctionType` has two direct formal parameters at
indices (`#0`, `#1`) and one direct formal result of tuple type:

```
SILFunctionType(foo): (#0: Int, #1: T) -> @out (Int, T)
```

Canonical SIL with opaque values matches `SILFunctionType`. The
definition of `foo` has two direct `SILFunctionArgument`s at (`#0`,
`#1`):

```
SILFunctionArguments: (#0: Int, #1: T) -> (Int, T)
```

The Lowered SIL for `foo`s definition has an indirect "result
argument" at index #0. The function parameter indices are now (`#1`,
`#2`):

```
SILFunctionArguments: (#0: *T, #1: Int, #2: T) -> Int
```

Creation of the closure has one applied argument at index `#0`. Note
that the first applied argument is actually the second operand (the
first is the callee), and in lowered SIL, it is actually the third
SILFunctionArgument (after the indirect result and first parameter):

```
%closure = partial_apply @foo(#0: t)
```

Application of the closure with opaque values has one applied
argument:

```
%resultTuple = apply %closure(#0: i)
```

Lowered application of the closure has two applied arguments:

```
%directResult = apply %closure(#0: %indirectResult: *T, #1: i)
```

The mapping between `SILFunctionType` and `SILFunctionArgument`, which depends
on the SIL stage, is managed by the
[SILFunctionConventions](https://github.com/apple/swift/blob/master/include/swift/SIL/SILFunctionConventions.h)
abstraction. This API follows naming conventions to communicate the meaning of the integer indices:

- "Parameters" refer to the function signature's tuple of arguments.

- "SILArguments" refer to the set of `SILFunctionArgument` in the callee's entry block, including any indirect results required by the current SIL stage.

These argument indices and their relative offsets should never be
hardcoded. Although this is common practice in LLVM, it should be
avoided in SIL: (a) the structure of SIL instructions, particularly
applies, is much more nuanced than LLVM IR, (b) assumptions that may
be valid at the initial point of use are often copied into parts of
the code where they are no longer valid; and (c) unlike LLVM IR, SIL
is not stable and will continue evolving.

Translation between SILArgument and parameter indices should use:
`SILFunctionConventions::getSILArgIndexOfFirstParam()`.

Translation between SILArgument and result indices should use:
`SILFunctionConventions::getSILArgIndexOfFirstIndirectResult()`.

Convenience methods exist for the most common uses, so there is
typically no need to use the above "IndexOfFirst" methods to translate
one integer index into another. The naming convention of the
convenience method should clearly indicate which form of index it
expects. For example, information about a parameter's type can be retrieved directly from a SILArgument index: `getParamInfoForSILArg(index)`, `getSILArgumentConvention(index)`, and `getSILArgumentType(index)`.

Another abstraction,
[`ApplySite`](https://github.com/search?utf8=✓&q=%22class+ApplySite%22+repo%3Aapple%2Fswift+path%3Ainclude%2Fswift%2FSIL&type=Code&ref=advsearch&l=&l=),
abstracts over the various kinds of `apply` instructions, including
`try_apply`, `begin_apply`, and `partial_apply`.

`ApplySite::getSubstCalleeConv()` is commonly used to query the
callee's `SILFunctionConventions` which provides information about the
function's type and its definition as explained above. Information about the applied arguments can be queried directly from the `ApplySite` API.

For example, `ApplySite::getAppliedArgumentConvention(index)` takes an
applied argument index, while
`SILFunctionArguments::getSILArgumentConvention(index`) takes a
`SILFunctionArgument` index. They both return the same information,
but from a different viewpoint.

A common mistake is to directly map the ApplySite's caller-side
arguments onto callee-side SILFunctionArguments. This happens to work
until the same code is exposed to a `partial_apply`. Instead, use the `ApplySite` API for applied argument indices, or use
`ApplySite::getCalleeArgIndexOfFirstAppliedArg()` to translate the
apply's arguments into function convention arguments.

Consistent use of common idioms for accessing arguments should be
adopted throughout the compiler. Plenty of bugs have resulted from
assumptions about the form of SIL made in one area of the compiler
that have been copied into other parts of the compiler. For example,
knowing that a block of code is guarded by a dynamic condition that
rules out PartialApplies is no excuse to conflate applied arguments
with function arguments. Also, without consistent use of common
idioms, it becomes overly burdensome to evolve these APIs over time.

## SILGen

TBD: Possibly link to a separate document explaining the architecture of SILGen.

Some information from SIL.rst could be moved here.

## IRGen

TBD: Possibly link to a separate document explaining the architecture of IRGen.

## SILAnalysis and the PassManager

TBD: describe the mechanism by which passes invalidate and update the
PassManager and its available analyses.

## High Level SIL Optimizations

[HighLevelSILOptimizations.rst](HighLevelSILOptimizations.rst) discusses how the optimizer imbues
certain special SIL types and SIL functions with higher level
semantics.


## Iterating over the Control Flow Graph (CFG) safely

Avoiding iterator invalidation is one of the most important things to pay
attention to when working on a compiler. Over time we are working on modifying
mutating utility functions to return a stable iterator appropriate for
processing the next instruction in a block. We are not there yet today so
sometimes one must reason about this explicitly.

With that in mind, we provide strategies below for doing so as a cook book for
new compiler writers. NOTE: One can only use worklists to large effect. Since
worklists are not "iterating" over the CFG, we do not include them here.

1. Read only analysis:

```
for (auto &block : function) {
  for (auto &inst : block) {
    ... do things ...
  }
}
```

2. Pass that only ever eliminates the current visited instruction (e.x. a
  combiner).

```
for (auto &block : function) {
  for (auto ii = block.begin(), ie = block.end(); ii != ie; ) {
    // Dereference the iterator and then move the iterator to the next
    // instruction. Since our instructions are in an iplist data structure,
    // deleting an instruction does not impact the next instruction in the
    // block.
    auto *inst = &*ii;
    ++ii;

    if (!safeToDelete(inst)) {
      continue;
    }

    inst->eraseFromParent();
  }
}
```

3. Iterating using reverse_iterators to implement a pass that ignores arguments
   and may erase the current instruction and any instructions dominated by the
   current instruction. This is done using reverse_iterators to ease the logic.

   NOTE: reverse iterators are offset by 1 and always return the previous
   element with respect to what they have stored.

```
for (auto &block : function) {
  // Has start stored, but represents a sentinel value.
  auto ii = block.rend();
  // Has end stored, but represents std::prev(end) unless block is empty (in
  // which case it is just the sentinel.
  auto start = block.rbegin();

  // If the block is empty, continue.
  if (start == ii)
    continue;

  // Go to the first instruction to process. This is equivalent to moving to the
  // first element of the array.
  --ii;

  // Then until we process the first instruction of the block...
  while (ii != start) {
    // Store into tmp a reverse iterator to the element /before/ ii.
    auto tmp = std::next(ii);

    // Then try to optimize.
    auto *inst = &*ii;
    if (safeToDelete(inst)) {
      ... Do some work that may erase inst or instructions dominated by inst ...

      // Then since ii was deleted, the next instruction to process is
      // guaranteed to be our actual next instruction.
      ii = std::prev(tmp);
      continue;
    }

    // Otherwise, we didn't delete ii. That means that the iterator was not
    // invalidated and the next instruction that we should process is the next
    // instruction, so we decrement our reverse iterator appropriately.
    --ii;
  }
}
```

4. Using (2), SILUndef, and a set of values to delete.

Often times in the face of complicated invalidation semantics, one can simplify
the code by iterating over the CFG using method (2) and adding any potentially
dead instructions to a vector. Then after one has finished processing all
instructions in the CFG, delete all instructions in the set.

```
SmallSetVector<SILInstruction *, 32> maybeDeadInsts;

for (auto &block : function) {
  for (auto ii = block.begin(), ie = block.end(); ii != ie; ) {
    // Dereference the iterator and then move the iterator to the next
    // instruction. Since our instructions are in an iplist data structure,
    // deleting an instruction does not impact the next instruction in the
    // block.
    auto *inst = &*ii;
    ++ii;

    if (!safeToDelete(inst)) {
      continue;
    }

    transform(inst->getUses(), std::back_inserter(maybeDeadInsts),
              [](Operand *op) -> SILInstruction * { return op->getUser(); });
    inst->replaceAllUsesOfAllResultsWithUndef();
    inst->eraseFromParent();
  }
}

while (!maybeDeadInsts.empty()) {
  auto *next = maybeDeadInsts->pop_back_val();
  recursivelyDeleteTriviallyDeadInstructions(
    next,
    [&](SILInstruction *i) { maybeDeadInsts.remove(i); });
}
```
