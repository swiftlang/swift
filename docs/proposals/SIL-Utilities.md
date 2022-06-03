# Utilities for SIL optimizer passes
This document lists a set of SIL utilities to be used by the Swift implementation of the SIL optimizer.

The purpose of this proposal is to do a better planning of what utilities we need in SIL optimizer passes.

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

## SSA Traversal Utilities

#### Basic Walkers
This is currently in progress. We'll probably have an up-walkers and  down-walkers for both, addresses and non-address values.

**Uses:** instruction classifications  
**Related C++ utilities:** `AccessPath`, `RCIdentityAnalysis`, various def-use/use-def walkers in optimization passes.  
**Status:** in progress

#### `SmallProjectionPath`
Describes a path of projections.

**Related C++ utilities:** `AccessPath`, `ProjectionPath`  
**Status:** done

#### `EscapeInfo`
Escape analysis, which is used e.g. in stack promotion or alias analysis.

**Uses:** basic walkers, `SmallProjectionPath`  
**Related C++ utilities:** `EscapeAnalysis`, various def-use walkers in optimization passes.  
**Status:** done, but factoring out the walkers is in progress

#### `AccessPath`
Details need to be decided.

**Uses:** basic walkers  
**Related C++ utilities:** `AccessPath`  
**Status:** to-do

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

## OSSA Utilities
#### `Value.makeAvailable()` and `Value.copy(at:)`
To be used where a value is copied in one block and used in another block.

**Uses:** `BasicBlockRange`  
**Related C++ utilities:** `makeValueAvailable()`, `OwnershipLifetimeExtender`  
**Status:** done
