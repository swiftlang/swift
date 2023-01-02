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
                     _ context: some MutatingContext, insertFunc: (Builder) -> ()) {
    if inst is TermInst {
      for succ in inst.parentBlock.successors {
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
  func makeAvailable(in destBlock: BasicBlock, _ context: some MutatingContext) -> Value {
    assert(uses.isEmpty)
    assert(ownership == .owned)

    let beginBlock = parentBlock
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
            _ context: some MutatingContext) -> Value {
    let builder = Builder(at: insertionPoint, context)
    let copiedValue = builder.createCopyValue(operand: self)
    return copiedValue.makeAvailable(in: destBlock, context)
  }
}

extension ProjectedValue {
  /// Returns true if the address can alias with `rhs`.
  ///
  /// Example:
  ///   %1 = struct_element_addr %s, #field1
  ///   %2 = struct_element_addr %s, #field2
  ///
  /// `%s`.canAddressAlias(with: `%1`) -> true
  /// `%s`.canAddressAlias(with: `%2`) -> true
  /// `%1`.canAddressAlias(with: `%2`) -> false
  ///
  func canAddressAlias(with rhs: ProjectedValue, _ context: some Context) -> Bool {
    // self -> rhs will succeed (= return false) if self is a non-escaping "local" object,
    // but not necessarily rhs.
    if !isEscaping(using: EscapesToValueVisitor(target: rhs), context) {
      return false
    }
    // The other way round: rhs -> self will succeed if rhs is a non-escaping "local" object,
    // but not necessarily self.
    if !rhs.isEscaping(using: EscapesToValueVisitor(target: self), context) {
      return false
    }
    return true
  }
}

private struct EscapesToValueVisitor : EscapeVisitor {
  let target: ProjectedValue

  mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
    if operand.value == target.value && path.projectionPath.mayOverlap(with: target.path) {
      return .abort
    }
    if operand.instruction is ReturnInst {
      // Anything which is returned cannot escape to an instruction inside the function.
      return .ignore
    }
    return .continueWalk
  }

  var followTrivialTypes: Bool { true }
  var followLoads: Bool { false }
}
