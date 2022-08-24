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

#### `SmallProjectionPath`
Describes a path of projections.

**Related C++ utilities:** `AccessPath`, `ProjectionPath`  
**Status:** done

#### `EscapeInfo`
Escape analysis, which is used e.g. in stack promotion or alias analysis.

**Uses:** Walk Utils, `SmallProjectionPath`  
**Related C++ utilities:** `EscapeAnalysis`, various def-use walkers in optimization passes.  
**Status:** done

#### Access Utils
A set of utilities for analyzing memory accesses. It defines the following concepts:
* `AccessBase`: represents the base address of a memory access.
* `AccessPath`: a pair of an `AccessBase` and `SmallProjectionPath` with the path describing the specific address (in terms of projections) of the access.
* `AccessStoragePath`: identifies the reference (or a value which contains a reference) an address originates from.

**Uses:** Walk utils
**Related C++ utilities:** `AccessPath`  and other access utilities.
**Status:** done

## Control- and Dataflow

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

