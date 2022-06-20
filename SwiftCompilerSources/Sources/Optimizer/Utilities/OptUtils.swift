//===--- OptUtils.swift - Utilities for optimizations ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension Value {
  var nonDebugUses: LazyFilterSequence<UseList> {
    uses.lazy.filter { !($0.instruction is DebugValueInst) }
  }
}

extension Builder {
  static func insert(after inst: Instruction, location: Location,
                     _ context: PassContext, insertFunc: (Builder) -> ()) {
    if inst is TermInst {
      for succ in inst.block.successors {
        assert(succ.hasSinglePredecessor,
               "the terminator instruction must not have critical successors")
        let builder = Builder(at: succ.instructions.first!, location: location,
                              context)
        insertFunc(builder)
      }
    } else {
      let builder = Builder(at: inst.next!, location: location, context)
      insertFunc(builder)
    }
  }
}

extension Value {
  /// Makes this new owned value available to be used in the block `destBlock`.
  ///
  /// Inserts required `copy_value` and `destroy_value` operations in case the `destBlock`
  /// is in a different control region than this value. For example, if `destBlock` is
  /// in a loop while this value is not in that loop, the value has to be copied for
  /// each loop iteration.
  func makeAvailable(in destBlock: BasicBlock, _ context: PassContext) -> Value {
    precondition(uses.isEmpty)
    precondition(ownership == .owned)

    let beginBlock = definingBlock
    var useToDefRange = BasicBlockRange(begin: beginBlock, context)
    defer { useToDefRange.deinitialize() }

    useToDefRange.insert(destBlock)

    // The value needs to be destroyed at every exit of the liferange.
    for exitBlock in useToDefRange.exits {
      let builder = Builder(at: exitBlock.instructions.first!, context)
      builder.createDestroyValue(operand: self)
    }
  
    if useToDefRange.contains(destBlock) {
      // The `destBlock` is within a loop, so we need to copy the value at each iteration.
      let builder = Builder(at: destBlock.instructions.first!, context)
      return builder.createCopyValue(operand: self)
    }
    return self
  }

  /// Copies this value at `insertionPoint` and makes the copy available to be used in `destBlock`.
  ///
  /// For details see `makeAvailable`.
  func copy(at insertionPoint: Instruction, andMakeAvailableIn destBlock: BasicBlock,
            _ context: PassContext) -> Value {
    let builder = Builder(at: insertionPoint, context)
    let copiedValue = builder.createCopyValue(operand: self)
    return copiedValue.makeAvailable(in: destBlock, context)
  }
}
