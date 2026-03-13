//===--- SimplifyRefCasts.swift -------------------------------------------===//
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

// Note: this simplifications are not SILCombineSimplifiable, because SILCombine has
// its own simplifications for those cast instructions which are not ported to Swift, yet.

extension CheckedCastBranchInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if removeIfDead(context) {
      return
    }

    // Has only an effect if the source is an (existential) reference.
    simplifySourceOperandOfRefCast(context)
  }
}

extension UncheckedRefCastInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    simplifySourceOperandOfRefCast(context)
  }
}

private extension CheckedCastBranchInst {

  /// Removes the `checked_cast_br` if it is dead:
  /// ```
  ///   checked_cast_br B in %1 to X, bb1, bb2
  /// bb1(%2 : @owned $X):
  ///   destroy_value %2         // no other instructions in this block
  ///   br bb3
  /// bb2(%4 : @owned $B):
  ///   destroy_value %4         // no other instructions in this block
  ///   br bb3
  /// ```
  ///
  func removeIfDead(_ context: SimplifyContext) -> Bool {
    guard let successTarget = getTargetOfEmpty(block: successBlock),
          let failureTarget = getTargetOfEmpty(block: failureBlock),
          successTarget == failureTarget
    else {
      return false
    }

    let builder = Builder(before: self, context)
    if source.ownership == .owned {
      builder.createDestroyValue(operand: source)
    }
    builder.createBranch(to: successTarget)
    context.erase(instruction: self)
    return true
  }
}

private func getTargetOfEmpty(block: BasicBlock) -> BasicBlock? {
  for inst in block.instructions {
    switch inst {
    case let destroy as DestroyValueInst:
      guard let blockArg = block.arguments.singleElement,
            destroy.destroyedValue == blockArg
      else {
        return nil
      }
    case let branch as BranchInst:
      guard branch.operands.isEmpty else {
        return nil
      }
      return branch.targetBlock
    default:
      return nil
    }
  }
  fatalError("didn't visit terminator instruction of block")
}

private extension UnaryInstruction {

  /// Look through `upcast` and `init_existential_ref` instructions and replace the
  /// operand of this cast instruction with the original value.
  /// For example:
  /// ```
  ///   %2 = upcast %1 : $Derived to $Base
  ///   %3 = init_existential_ref %2 : $Base : $Base, $AnyObject
  ///   checked_cast_br AnyObject in %3 : $AnyObject to Derived, bb1, bb2
  /// ```
  ///
  /// This makes it more likely that the cast can be constant folded because the source
  /// operand's type is more accurate. In the example above, the cast reduces to
  /// ```
  ///   checked_cast_br Derived in %1 : $Derived to Derived, bb1, bb2
  /// ```
  /// which can be trivially folded to always-succeeds.
  ///
  func simplifySourceOperandOfRefCast(_ context: SimplifyContext) {
    while true {
      switch operand.value {
      case let ier as InitExistentialRefInst:
        if !tryReplaceSource(withOperandOf: ier, context) {
          return
        }
      case let uc as UpcastInst:
        if !tryReplaceSource(withOperandOf: uc, context) {
          return
        }
      default:
        return
      }
    }

  }

  func tryReplaceSource(withOperandOf inst: SingleValueInstruction, _ context: SimplifyContext) -> Bool {
    let singleUse = context.preserveDebugInfo ? inst.uses.singleUse : inst.uses.ignoreDebugUses.singleUse
    let canEraseInst = singleUse?.instruction == self
    let replacement = inst.operands[0].value

    if parentFunction.hasOwnership {
      if !canEraseInst && replacement.ownership == .owned {
        // We cannot add more uses to `replacement` without inserting a copy.
        return false
      }

      operand.set(to: replacement, context)

      if let ccb = self as? CheckedCastBranchInst {
        // In OSSA, the source value is passed as block argument to the failure block.
        // We have to re-create the skipped source instruction in the failure block.
        insertCompensatingInstructions(for: inst, in: ccb.failureBlock, context)
      }
    } else {
      operand.set(to: replacement, context)
    }

    if let ccb = self as? CheckedCastBranchInst {
        // Make sure that updating the formal type with the operand type is
        // legal.
        if operand.value.type.isLegalFormalType {
            ccb.updateSourceFormalTypeFromOperandLoweredType()
        }
    }
    if canEraseInst {
      context.erase(instructionIncludingDebugUses: inst)
    }
    return true
  }
}

/// Compensate a removed source value instruction in the failure block.
/// For example:
/// ```
///   %inst = upcast %sourceValue : $Derived to $Base
///   checked_cast_br Base in %inst : $Base to Derived, success_block, failure_block
///   ...
/// failure_block(%oldArg : $Base):
/// ```
/// is converted to:
/// ```
///   checked_cast_br Derived in %sourceValue : $Derived to Derived, success_block, failure_block
///   ...
/// failure_block(%newArg : $Derived):
///   %3 = upcast %newArg : $Derived to $Base
/// ```
private func insertCompensatingInstructions(for inst: Instruction, in failureBlock: BasicBlock, _ context: SimplifyContext) {
  assert(failureBlock.arguments.count == 1)
  let sourceValue = inst.operands[0].value
  let newArg = failureBlock.addArgument(type: sourceValue.type, ownership: sourceValue.ownership, context)
  let builder = Builder(atBeginOf: failureBlock, context)
  let newInst: SingleValueInstruction
  switch inst {
  case let ier as InitExistentialRefInst:
    newInst = builder.createInitExistentialRef(instance: newArg,
                                               existentialType: ier.type,
                                               formalConcreteType: ier.formalConcreteType,
                                               conformances: ier.conformances)
  case let uc as UpcastInst:
    newInst = builder.createUpcast(from: newArg, to: uc.type)
  default:
    fatalError("unhandled instruction")
  }
  let oldArg = failureBlock.arguments[0]
  oldArg.uses.replaceAll(with: newInst, context)
  failureBlock.eraseArgument(at: 0, context)
}
