//===--- EagerMaterialization.swift ----------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

/// Eagerly materialize values that are stored locally inside loops.
///
/// If a value is stored to a local alloc_stack within an inner loop, but the
/// value itself is available before/outside the loop, store the value to a
/// stack allocation at the point that it becomes available. Replace the store
/// with a copy_addr from this "eager" temporary allocation. This transformation
/// enables transformations like TempRValueElimination to eliminate repeated
/// stores to the stack within loops.
///
/// ```
/// bb0:
///   br bb1
/// bb1:
///   cond_br %cond, bb2, bb3
/// bb2:
///   %temp = alloc_stack $T
///   store %val to [init] %temp
///   ...
///   dealloc_stack %temp
///   br bb1
/// bb3:
///   ...
/// ```
/// ->
/// ```
/// bb0:
///   %eager = alloc_stack $T
///   store %val to [init] %eager
///   br bb1
/// bb1:
///   cond_br %cond, bb2, bb3
/// bb2:
///   %temp = alloc_stack $T
///   copy_addr %eager to [init] %temp
///   ...
///   dealloc_stack %temp
///   br bb1
/// bb3:
///   ...
/// ```
///
let eagerMaterialization = FunctionPass(name: "eager-materialization") {
  (function: Function, context: FunctionPassContext) in

  var materialization = EagerMaterialization(for: function, context)
  defer { materialization.deinitialize() }
  materialization.materialize()
}

private struct EagerMaterialization {

  let context: FunctionPassContext
  var visited: BasicBlockSet
  var function: Function
  // The stack allocations created to eagerly materialize values, that must be
  // deallocated in reverse order at the end of the function.
  var allocStacks: [AllocStackInst]
  var materializedAddresses: [HashableValue: Value]
  let allocBuilder: Builder

  init(for function: Function, _ context: FunctionPassContext) {
    self.context = context
    self.visited = BasicBlockSet(context)
    self.function = function
    self.allocStacks = []
    self.materializedAddresses = [:]
    self.allocBuilder = Builder(atBeginOf: function.entryBlock, context)
  }

  /// Perform eager materialization for each top-level loop.
  mutating func materialize() {
    for loop in context.loopTree.loops {
      materialize(in: loop)
    }
  }

  /// Perform eager materialization, in a post-order DFS over the loop tree.
  /// This order ensures that each block is visited while processing the
  /// innermost loop it is part of.
  private mutating func materialize(in loop: Loop) {
    for inner in loop.innerLoops {
      materialize(in: inner)
    }

    for block in loop.loopBlocks where !visited.contains(block) {
      defer { visited.insert(block) }

      // Eager materialization introduces additional copies of the value, so it
      // is only possible when the value is trivial, and adding a new store
      // would not cause it to be consumed.
      for case let store as StoreInst in block.instructions
      where store.storeOwnership == .trivial {

        // Eagerly materialize only when the stored value exists outside the
        // block's innermost loop, and the destination address is a stack
        // allocation within the innermost loop. These are prime candidates for
        // TempRValueElimination.
        guard let destInst = store.destination.definingInstruction,
          case let alloc as AllocStackInst = destInst,
          loop.contains(block: alloc.parentBlock)
        else {
          continue
        }

        let storedValue = store.source

        if loop.contains(block: storedValue.parentBlock) {
          continue
        }

        // Materialize the stored value into an address and replace the store
        // with a copy_addr.
        let address = getMaterializedAddress(of: storedValue)
        Builder(after: store, context).createCopyAddr(
          from: address, to: store.destinationAddress, initializeDest: true)

        context.erase(instruction: store)
      }
    }
  }

  /// Get a materialized address for the supplied value.
  private mutating func getMaterializedAddress(of value: Value) -> Value {
    if let address = materializedAddresses[value.hashable] {
      return address
    }

    // Allocate the address for the value in the entry block, and store it there
    // at the earliest point where the value and address are both available.
    assert(value.type.isObject, "Only values with object type should be stored on the stack.")
    let alloc = allocBuilder.createAllocStack(value.type)
    allocStacks.append(alloc)
    materializedAddresses[value.hashable] = alloc
    let insertionPoint = getStoreInsertionPoint(for: value, after: alloc)
    Builder(before: insertionPoint, context).createStore(
      source: value, destination: alloc, ownership: .trivial)

    return alloc
  }

  /// Given a value that we need to eagerly materialize, find a point where we
  /// can insert a store. Returns an instruction we can insert the store before.
  /// The insertion point must be after the supplied alloc_stack (which must be
  /// in the entry block).
  private func getStoreInsertionPoint(for value: Value, after alloc: AllocStackInst) -> Instruction
  {
    precondition(alloc.parentBlock == alloc.parentFunction.entryBlock)

    let nextInstruction = value.nextInstruction

    if nextInstruction.parentBlock != alloc.parentBlock {
      // The nextInstruction is in a non-entry block, so it must be after the alloc_stack.
      return nextInstruction
    }

    // We must insert the store after the allocation and the point where the value is introduced.
    if alloc.strictlyDominatesInBlock(nextInstruction) {
      return nextInstruction
    }
    // alloc_stack is not a TermInst so next! is safe.
    return alloc.next!

  }

  public mutating func deinitialize() {
    let dea = context.deadEndBlocks
    for block in function.blocks
        where block.isReachableExitBlock && !dea.isDeadEnd(block) {
      let deallocBuilder = Builder(before: block.terminator, context)
      for alloc in allocStacks.reversed() {
        deallocBuilder.createDeallocStack(alloc)
      }
    }

    visited.deinitialize()
  }
}
