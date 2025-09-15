# Utilities for SIL optimizer passes
This document lists a set of SIL utilities to be used by the Swift implementation of the SIL optimizer.

Some of the utilities presented in this document are still in the design phase and are not implemented yet (see **Status**).

### Goals for utilities
We want to avoid a situation like we have in the C++ SIL optimizer, where a huge amount of (partly) redundant utilities exist. Many of those utilities have overlapping functionality,  are difficult to discover and are poorly designed. We want to do better in the Swift SIL optimizer.

## Basic Data-Structures

#### `Stack`

An allocation-free, array like data structure. To be used instead of `Swift.Array` wherever random-access is not needed.

**Related C++ utilities:** `llvm::SmallVector`, `Stack`
**Status:** done

#### `BasicBlockSet`
An extremely efficient implementation of a set of basic blocks.

**Related C++ utilities:** `BasicBlockSet`
**Status:** done

#### `ValueSet`
An extremely efficient implementation of a set of values.

**Related C++ utilities:** `ValueSet`
**Status:** done

#### `InstructionSet`
An extremely efficient implementation of a set of instructions.

**Related C++ utilities:** `InstructionSet `
**Status:** done

#### `BasicBlockWorklist`
To be used for all kind of basic-block work-list algorithms.

**Uses:** `Stack`, `BasicBlockSet`  
**Related C++ utilities:** `BasicBlockWorklist`
**Status:** done

## SIL data structures

#### `SmallProjectionPath`
Describes a path of projections.

**Related C++ utilities:** `AccessPath`, `ProjectionPath`  
**Status:** done

### `ProjectedValue`
A projected value is defined by the original value and a projection path.

**Related C++ utilities:** `AccessPath`
**Status:** done

### `SingleInlineArray`

An array with the first element stored inline. Useful for analyses that produces results that are typically a 1-to-1 map, but rarely a 1-to-N map.

## Building SIL

#### `static Builder.insert(after:, insertFunc: (Builder) -> ())`
Useful to insert instructions after a (potential) terminator instruction.

**Related C++ utilities:** `SILBuilderWithScope::insertAfter()`
**Status:** done

## SSA Traversal Utilities

#### Walk Utils
This consists of four protocols, which can be implemented to walk up or down the SSA graph:

* `ValueDefUseWalker`
* `AddressDefUseWalker`
* `ValueUseDefWalker`
* `AddressUseDefWalker`

**Uses:** instruction classifications  
**Related C++ utilities:** `AccessPath`, `RCIdentityAnalysis`, various def-use/use-def walkers in optimization passes.  
**Status:** done

#### Escape Utilities
Escape analysis, which is used e.g. in stack promotion or alias analysis.
Escape analysis is usable through the following methods of `ProjectedValue` and `Value`:

* `isEscaping()`
* `isAddressEscaping()`
* `visit()`
* `visitAddress()`

**Uses:** Walk Utilities, `ProjectedValue`
**Related C++ utilities:** `EscapeAnalysis`, various def-use walkers in optimization passes.  
**Status:** done

#### Access Utils
A set of utilities for analyzing memory accesses. It defines the following concepts:

* `AccessBase`: represents the base address of a memory access.
* `AccessPath`: a pair of an `AccessBase` and `SmallProjectionPath` with the path describing the specific address (in terms of projections) of the access.
* Access storage path (which is of type `ProjectedValue`): identifies the reference - or a value which contains a reference - an address originates from.

**Uses:** Walk utils
**Related C++ utilities:** `AccessPath`  and other access utilities.
**Status:** done

### Address Utils

* `AddressUseVisitor`: classify address uses. This can be used by def-use walkers to ensure complete handling of all legal SIL patterns.

**Related Swift Utilities**
`AddressDefUseWalker`

**Related C++ Utilities**
`Projection::isAddressProjection`
`isAccessStorageCast`
`transitiveAddressWalker`

TODO: Refactor AddressDefUseWalker to implement AddressUseVisitor.

### Ownership Utils

#### BorrowUtils.swift has utilities for traversing borrow scopes:

* `BorrowingInstruction`: find borrow scopes during def-use walks
* `BeginBorrowValue`: find borrow scopes during use-def walks
* `gatherBorrowIntroducers`: use-def walk finds the current scopes
* `gatherEnclosingValues`: use-def walk finds the outer lifetimes that enclose the current scope

#### OwnershipUtils.swift has utilities for traversing OSSA lifetimes:

* `computeLinearLiveness`: compute an InstructionRange from the immediate lifetime ending uses.
* `computeInteriorLiveness`: complete def-use walk to compute an InstructionRange from all transitive use points that must be within an OSSA lifetime.
* `InteriorUseWalker`: def-use walker for all transitive use points that must be within an OSSA lifetime.
* `OwnershipUseVistor`: categorize all uses of an owned or guaranteed use by ownership effect. Use this within a recursive def-use walker to decide how to follow each use.

`InteriorUseWalker`, like `AddressDefUseWalker`, walks def-use address projections. The difference is that it visits and classifies all uses regardless of whether they are projections, it has callbacks for handling inner scopes, and it automatically handles the lifetime effect of inner scopes and dependent values.

#### ForwardingUtils.swift has utilities for traversing forward-extended  lifetimes:

Forward-extended lifetimes may include multiple OSSA lifetimes joined by ForwardingInstructions. Querying certain information about OSSA lifetimes, such as whether it has a lexical lifetime or a pointer escape, requires finding the introducer of the forward-extended lifetime. Forwarding walkers traverse the SSA graph of ForwardingInstructions:

* `ForwardingUseDefWalker`: Find the introducer of a forward-extended lifetime
* `ForwardingDefUseWalker`: Find all OSSA lifetimes within a forward-extended lifetime.

#### LifetimeDependence

Model lifetime dependencies in SIL, as required be ~Escapable types.

## Control- and Dataflow

#### `DeadEndBlocks`
A utility for finding dead-end blocks.
Dead-end blocks are blocks from which there is no path to the function exit (`return`, `throw` or unwind).
These are blocks which end with an unreachable instruction and blocks from which all paths end in "unreachable" blocks.

**Uses:** `BasicBlockWorklist`  
**Related C++ utilities:** `DeadEndBlocks`  
**Status:** done

#### `BasicBlockRange`
Defines a range from a dominating "begin" block to one or more "end" blocks. To be used for all kind of backward block reachability analysis.

**Uses:** `Stack`, `BasicBlockSet`, `BasicBlockWorklist`  
**Related C++ utilities:** `PrunedLiveBlocks`, `findJointPostDominatingSet()`  
**Status:** done

#### `InstructionRange`
Like `BasicBlockRange`, but at the granularity of instructions.

**Uses:** `BasicBlockRange`  
**Related C++ utilities:** `PrunedLiveness`, `ValueLifetimeAnalysis`  
**Status:** done

## Utilities for Inter-procedural Analysis

### `FunctionUses`
Provides a list of instructions, which reference a function. This utility performs an analysis of all functions in the module and collects instructions which reference other functions. It can be used to do inter-procedural caller-analysis.

**Related C++ utilities:** `CallerAnalysis` 
**Status:** done

## OSSA Utilities

#### `Value.makeAvailable()` and `Value.copy(at:)`
To be used where a value is copied in one block and used in another block.

**Uses:** `BasicBlockRange`  
**Related C++ utilities:** `makeValueAvailable()`, `OwnershipLifetimeExtender`  
**Status:** done

## Instruction classifications
We want to classify certain instructions into common groups so that passes can deal deal with the group instead of individual instruction opcodes. Optimization passes then don't need to be updated if new instructions are added to a group.

In Swift we can easily do this by introducing protocols to which instruction classes can conform to.

#### `ApplySite`

**Related C++ utilities:** `ApplySite`
**Status:** exists; to-do: complete member variables/functions  

#### `CastInstruction`
Details need to be decided.

**Conforming instructions:** `UpcastInst`, `UncheckedRefCastInst`, etc.  
**Members:** ownership behavior, e.g. forwarding, etc.   
**Status:** to-do  

#### `AddressProjectionInstruction`
Details need to be decided.

**Conforming instructions:** `StructElementAddrInst`, `TupleElementAddrInst`  
**Members:** `var fieldIndex: Int`  
**Related C++ utilities:** `skipAddrProjections`  
**Status:** to-do  

#### `AggregateInstruction`
Details need to be decided.

**Conforming instructions:** `StructInst`, `TupleInst`  
**Status:** to-do  

#### `ExtractInstruction`
Details need to be decided.

**Conforming instructions:** `StructExtractInst`, `TupleExtractInst`  
**Status:** to-do  

#### `BorrowScope`
Details need to be decided.
Maybe it's better to not do this as a protocol but just add an extension function `Value.introducesBorrowScope`. Reason: an `Argument` can be guaranteed or owned.

**Conforming instructions:** `BeginBorrowInst`, `Argument` with guaranteed ownership (however will do that), `LoadBorrowInst`  
**Status:** to-do  

