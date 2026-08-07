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

    if let phi = Phi(destroyedValue) {
      tryHoistDestroysIntoPredecessors(of: phi, context)
      return
    }

    if let forwardingInst = destroyedValue.asOptimizableForwardingInstruction {
      tryRemoveForwardingInstruction(forwardingInst, context)
    }
  }

  /// Moves a `destroy_value` of a phi argument to the phi's predecessor blocks.
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
  private func tryHoistDestroysIntoPredecessors(of phi: Phi, _ context: SimplifyContext) {
    let phiBlock = phi.value.parentBlock

    guard phiBlock == parentBlock,
          // Dead-end destroys are no-ops, anyway. Don't try to move them away from an `unreachable` instruction.
          !isDeadEnd,
          phi.value.isOnlyDestroyed(ignoringFixLifetime: false, context),
          !isDeinitBarrierInBlock(before: self, context)
    else {
      return
    }

    for incomingOp in phi.incomingOperands {
      Builder(before: incomingOp.instruction, context).createDestroyValue(operand: incomingOp.value)
    }

    // Users of `phi` include `debug_value` instructions and this `destroy_value`
    context.erase(instructions: phi.value.users)

    erasePhiArgument(phi: phi, context)
  }
}

/// If `forwardingInst` has no other users than `destroy_value` (except `fix_lifetime` and `debug_value`),
/// remove the `forwardingInst` and destroy its (owned) operands instead.
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
private func tryRemoveForwardingInstruction(_ forwardingInst: SingleValueInstruction, _ context: SimplifyContext) {

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
  guard forwardingInst.isOnlyDestroyed(ignoringFixLifetime: true, context) else {
    return
  }

  for user in forwardingInst.users {
    switch user {
    case is DebugValueInst:
      break
    case let destroy as DestroyValueInst:
      let builder = Builder(before: destroy, context)
      for op in forwardingInst.definedOperands where op.value.ownership == .owned {
        builder.createDestroyValue(operand: op.value, isDeadEnd: destroy.isDeadEnd)
      }
    case let fixLifetime as FixLifetimeInst:
      let builder = Builder(before: fixLifetime, context)
      for op in forwardingInst.definedOperands where op.value.ownership == .owned {
        builder.createFixLifetime(operand: op.value)
      }
    default:
      fatalError("unexpected user")
    }
  }

  // Users include `destroy_value`, `fix_lifetime` and `debug_value` instructions.
  context.erase(instructionIncludingAllUsers: forwardingInst)
}

private extension Value {
  func isOnlyDestroyed(ignoringFixLifetime: Bool, _ context: SimplifyContext) -> Bool {
    for user in users {
      switch user {
      case is DestroyValueInst:
        break
      case is DebugValueInst where !context.preserveDebugInfo:
        break
      case is FixLifetimeInst where ignoringFixLifetime:
        break
      default:
        return false
      }
    }
    return true
  }

  /// Return this value as a SingleValueInstruction if it is a forwarding instruction which can
  /// be optimized by `tryRemoveForwardingInstruction`.
  var asOptimizableForwardingInstruction: SingleValueInstruction? {
    switch self {
    case is StructInst,
         is EnumInst:
      guard type.nominal!.valueTypeDestructor == nil else {
        // Moving the destroy to a non-copyable struct/enum's operands would drop the deinit call!
        return nil
      }
      return self as? SingleValueInstruction

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
      return self as? SingleValueInstruction

    default:
      return nil
    }
  }
}

private func isDeinitBarrierInBlock(before instruction: Instruction, _ context: SimplifyContext) -> Bool {
  return ReverseInstructionList(first: instruction.previous).contains(where: {
    $0.isDeinitBarrier(context.calleeAnalysis)
  })
}
