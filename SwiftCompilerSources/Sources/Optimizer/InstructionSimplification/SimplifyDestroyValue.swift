//===--- SimplifyDestroyValue.swift ---------------------------------------===//
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

extension DestroyValueInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    // If the value has `.none` ownership, the destroy is a no-op. Note that a value can have `.none`
    // ownership even if it's type is not trivial, e.g.
    //
    // ```
    //   %1 = enum $NonTrivialEnum, #NonTrivialEnum.trivialCase!enumelt  // ownership: none
    //   %2 = destroy_value %1
    // ```
    //
    if destroyedValue.ownership == .none {
      context.erase(instruction: self)
      return
    }

    tryRemoveForwardingOperandInstruction(context)
  }

  /// Attempt to optimize by forwarding the destroy to operands of forwarding instructions.
  ///
  /// ```
  ///   %3 = struct $S (%1, %2)
  ///   destroy_value %3         // the only use of %3
  /// ```
  /// ->
  /// ```
  ///   destroy_value %1
  ///   destroy_value %2
  /// ```
  ///
  /// The benefit of this transformation is that the forwarding instruction can be removed.
  ///
  private func tryRemoveForwardingOperandInstruction(_ context: SimplifyContext) {
    switch destroyedValue {
    case is StructInst,
         is EnumInst:
      if destroyedValue.type.nominal!.valueTypeDestructor != nil {
        // Moving the destroy to a non-copyable struct/enum's operands would drop the deinit call!
        return
      }

    // Handle various "forwarding" instructions that simply pass through values
    // without performing operations that would affect destruction semantics.
    //
    // We are intentionally _not_ handling `unchecked_enum_data`, because that would not necessarily be
    // a simplification, because destroying the whole enum is more effort than to destroy an enum payload.
    // We are also not handling `destructure_struct` and `destructure_tuple`. That would end up in
    // an infinite simplification loop in MandatoryPerformanceOptimizations because there we "split" such
    // destroys again when de-virtualizing deinits of non-copyable types.
    //
    case is TupleInst,
         is RefToBridgeObjectInst,
         is ConvertFunctionInst,
         is ThinToThickFunctionInst,
         is UpcastInst,
         is UncheckedRefCastInst,
         is UnconditionalCheckedCastInst,
         is BridgeObjectToRefInst,
         is InitExistentialRefInst,
         is OpenExistentialRefInst:
      break

    case let arg as Argument:
      if isDeadEnd {
        // Dead-end destroys are no-ops, anyway. Don't try to move them away from an `unreachable` instruction.
        return
      }
      guard destroyedValue.hasSingleUse(ignoringFixLifetime: false, context) else {
        return
      }
      tryRemovePhiArgument(arg, context)
      return
      
    default:
      return
    }

    // Support fix_lifetime as use:
    // ```
    //   %3 = struct $S (%1, %2)
    //   fix_lifetime %3
    //   destroy_value %3         // the only use of %3, except `fix_lifetime`
    // ```
    // ->
    // ```
    //   fix_lifetime %1
    //   fix_lifetime %2
    //   destroy_value %1
    //   destroy_value %2
    // ```
    guard destroyedValue.hasSingleUse(ignoringFixLifetime: true, context) else {
      return
    }
    let destroyedInst = destroyedValue as! SingleValueInstruction

    let builder = Builder(before: self, context)
    for op in destroyedInst.definedOperands where op.value.ownership == .owned {
      builder.createDestroyValue(operand: op.value, isDeadEnd: isDeadEnd)
    }
    for fixLifetime in destroyedValue.uses.users(ofType: FixLifetimeInst.self) {
      let builder = Builder(before: fixLifetime, context)
      for op in destroyedInst.definedOperands where op.value.ownership == .owned {
        builder.createFixLifetime(operand: op.value)
      }
    }

    // Users include `debug_value` instructions and this `destroy_value`
    context.erase(instructionIncludingAllUsers: destroyedInst)
  }

  /// Handles the optimization of `destroy_value` instructions for phi arguments.
  /// This is a more complex case where the destroyed value comes from different predecessors
  /// via a phi argument. The optimization moves the `destroy_value` to each predecessor block.
  ///
  /// ```
  /// bb1:
  ///   br bb3(%0)
  /// bb2:
  ///   br bb3(%1)
  /// bb3(%3 : @owned T):
  ///   ...                // no deinit-barriers
  ///   destroy_value %3   // the only use of %3
  /// ```
  /// ->
  /// ```
  /// bb1:
  ///   destroy_value %0
  ///   br bb3
  /// bb2:
  ///   destroy_value %1
  ///   br bb3
  /// bb3:
  ///   ...
  /// ```
  ///
  private func tryRemovePhiArgument(_ arg: Argument, _ context: SimplifyContext) {
    guard let phi = Phi(arg),
          arg.parentBlock == parentBlock,
          !isDeinitBarrierInBlock(before: self, context)
    else {
      return
    }
    
    for incomingOp in phi.incomingOperands {
      let oldBranch = incomingOp.instruction as! BranchInst
      let builder = Builder(before: oldBranch, context)
      builder.createDestroyValue(operand: incomingOp.value)
      builder.createBranch(to: parentBlock, arguments: oldBranch.arguments(excluding: incomingOp))
      context.erase(instruction: oldBranch)
    }
    
    // Users of `arg` include `debug_value` instructions and this `destroy_value`
    context.erase(instructions: arg.uses.users)

    arg.parentBlock.eraseArgument(at: arg.index, context)
  }
}

private extension Value {
  func hasSingleUse(ignoringFixLifetime: Bool, _ context: SimplifyContext) -> Bool {
    var foundRelevantUse = false
    for use in uses {
      switch use.instruction {
      case is DebugValueInst where !context.preserveDebugInfo:
        break
      case is FixLifetimeInst where ignoringFixLifetime:
        break
      default:
        if foundRelevantUse {
          return false
        }
        foundRelevantUse = true
      }
    }
    return foundRelevantUse
  }
}


private func isDeinitBarrierInBlock(before instruction: Instruction, _ context: SimplifyContext) -> Bool {
  return ReverseInstructionList(first: instruction.previous).contains(where: {
    $0.isDeinitBarrier(context.calleeAnalysis)
  })
}

private extension BranchInst {
  func arguments(excluding excludeOp: Operand) -> [Value] {
    return Array(operands.filter{ $0 != excludeOp }.values)
  }
}
