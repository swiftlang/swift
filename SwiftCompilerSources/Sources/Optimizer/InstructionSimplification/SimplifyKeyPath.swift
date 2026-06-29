//===--- SimplifyKeyPath.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Removes a `keypath` instruction that has no "real" users.
///
/// A `keypath` instruction materializes a `KeyPath` object at runtime. If the resulting object is
/// never actually used - i.e. its only (transitive) users are ownership/lifetime instructions -
/// the instruction (together with all of those dead users) can be deleted.
///
/// The captured subscript-argument operands of `keypath` are consumed by the
/// instruction. When the `keypath` is removed those operands must instead be
/// destroyed at the locations where the keypath result itself used to be
/// destroyed, so the operand lifetimes still end at the same points.
///
/// ```
///   %arg = ... : $NonTrivial
///   %kp  = keypath $KeyPath<Root, Value>, (...) <Root, Value> (%arg)
///   destroy_value %kp
/// ```
/// ->
/// ```
///   %arg = ... : $NonTrivial
///   destroy_value %arg
/// ```
extension KeyPathInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    guard parentFunction.hasOwnership else {
      return
    }

    // If any operand owns a non-trivial value, we will have to insert compensating destroys for
    // it at every destroy site of the keypath. In that case we cannot tolerate `copy_value` users
    // of the keypath: a copy would extend the keypath's lifetime past the original destroy points,
    // and inserting operand destroys at those points would consume the operand twice.
    let needDestroyOperands = operands.contains { !$0.value.type.isTrivial(in: parentFunction) }

    // Collected destroys of the keypath result (and of values forwarded from it). These are the
    // locations where we will insert the compensating destroys for non-trivial operands.
    var destroys = Stack<DestroyValueInst>(context)
    defer { destroys.deinitialize() }

    guard allUsesRemovable(instruction: self, acceptCopies: !needDestroyOperands, destroys: &destroys, context) else {
      return
    }

    // For each non-trivial operand, re-create its destroy at every location where the keypath used
    // to be destroyed, so the operand's lifetime still ends in the same places.
    for operand in self.operands where !operand.value.type.isTrivial(in: parentFunction) {
      if operand.value.type.isAddress {
        // The keypath takes the value out of an address operand, leaving the original storage
        // uninitialized. We can't just destroy the original address at the keypath's destroy
        // sites, so copy the value (with take) into a fresh stack slot up-front, and destroy
        // that stack slot at each former destroy point.
        let allocBuilder = Builder(before: self, context)
        let stack = allocBuilder.createAllocStack(operand.value.type.objectType)
        allocBuilder.createCopyAddr(from: operand.value, to: stack,
                                                      takeSource: true, initializeDest: true)
        for destroy in destroys {
          let builder = Builder(before: destroy, context)
          builder.createDestroyAddr(address: stack)
          builder.createDeallocStack(stack)
          context.notifyInvalidatedStackNesting()
        }
      } else {
        // For object operands a plain `destroy_value` at each former destroy site is sufficient.
        for destroy in destroys {
          let builder = Builder(before: destroy, context)
          builder.createDestroyValue(operand: operand.value)
        }
      }
    }

    // Finally, erase the keypath together with all of its (now provably dead) users.
    context.erase(instructionIncludingAllUsers: self)
  }
}

/// Walks the def-use chain starting at `instruction` and returns true if
/// every transitive user can be erased together with the `keypath`.
///
/// All encountered `destroy_value`s are appended to `destroys`.
fileprivate func allUsesRemovable(instruction: Instruction,
                                  acceptCopies: Bool,
                                  destroys: inout Stack<DestroyValueInst>,
                                  _ context: SimplifyContext
) -> Bool {
  for result in instruction.results {
    for use in result.uses {
      switch use.instruction {
      case is CopyValueInst:
        // A `copy_value` is only safe to drop if we won't need to insert operand destroys at the
        // keypath's destroy sites (see the `needDestroyOperands` reasoning in `simplify`).
        if !acceptCopies {
          return false
        }
        fallthrough

      case is UpcastInst,
           is BeginBorrowInst,
           is EndBorrowInst,
           is MoveValueInst,
           // Dead `class_method` instructions can be a leftover from optimizing keypath applications.
           is ClassMethodInst:
        if !allUsesRemovable(instruction: use.instruction, acceptCopies: acceptCopies, destroys: &destroys, context) {
          return false
        }

      case let destroy as DestroyValueInst:
        destroys.append(destroy)

      case is DebugValueInst:
        if context.preserveDebugInfo {
          return false
        }

      default:
        return false
      }
    }
  }
  return true
}
