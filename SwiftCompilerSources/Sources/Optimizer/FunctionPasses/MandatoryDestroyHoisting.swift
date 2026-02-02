//===--- MandatoryDestroyHoisting.swift ------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Hoists `destroy_value` instructions for non-lexical values.
///
/// ```
///   %1 = some_ownedValue
///   ...
///   last_use(%1)
///   ... // other instructions
///   destroy_value %1
/// ```
/// ->
/// ```
///   %1 = some_ownedValue
///   ...
///   last_use(%1)
///   destroy_value %1    // <- moved after the last use
///   ... // other instructions
/// ```
///
/// In contrast to non-mandatory optimization passes, this is the only pass which hoists destroys
/// over deinit-barriers. This ensures consistent behavior in -Onone and optimized builds.
///
///
let mandatoryDestroyHoisting = FunctionPass(name: "mandatory-destroy-hoisting") {
  (function: Function, context: FunctionPassContext) in

  var endAccesses = Stack<EndAccessInst>(context)
  defer { endAccesses.deinitialize() }
  endAccesses.append(contentsOf: function.instructions.compactMap{ $0 as? EndAccessInst })

  for block in function.blocks {
    for arg in block.arguments {
      hoistDestroys(of: arg, endAccesses: endAccesses, context)
      if !context.continueWithNextSubpassRun() {
        return
      }
    }
    for inst in block.instructions {
      for result in inst.results {
        hoistDestroys(of: result, endAccesses: endAccesses, context)
        if !context.continueWithNextSubpassRun(for: inst) {
          return
        }
      }
    }
  }
}

private func hoistDestroys(of value: Value, endAccesses: Stack<EndAccessInst>, _ context: FunctionPassContext) {
  guard value.ownership == .owned,

        // We must not violate side-effect dependencies of non-copyable deinits.
        // Therefore we don't handle non-copyable values.
        !value.type.isMoveOnly,

        // Just a shortcut to avoid all the computations if there is no destroy at all.
        !value.uses.users(ofType: DestroyValueInst.self).isEmpty,

        // Hoisting destroys is only legal for non-lexical lifetimes.
        !value.isInLexicalLiverange(context),

        // Avoid compromimsing debug-info in Onone builds for source-level variables with non-lexical lifetimes.
        // For example COW types, like Array, which are "eager-move" and therefore not lexical.
        !needPreserveDebugInfo(of: value, context)
  else {
    return
  }

  guard var liverange = Liverange(of: value, context) else {
    return
  }
  defer { liverange.deinitialize() }

  // We must not move a destroy into an access scope, because the deinit can have an access scope as well.
  // And that would cause a false exclusivite error at runtime.
  liverange.extendWithAccessScopes(of: endAccesses)

  var aliveDestroys = insertNewDestroys(of: value, in: liverange)
  defer { aliveDestroys.deinitialize() }

  removeOldDestroys(of: value, ignoring: aliveDestroys, context)
}

private func insertNewDestroys(of value: Value, in liverange: Liverange) -> InstructionSet {
  var aliveDestroys = InstructionSet(liverange.context)

  // The liverange, excluding regions which end up in `destroy_value [dead_end]`.
  var nonDeadEndRange = BasicBlockRange(begin: value.parentBlock, liverange.context)
  defer { nonDeadEndRange.deinitialize() }
  nonDeadEndRange.insert(contentsOf: value.uses.users(ofType: DestroyValueInst.self)
                                          .filter{ !$0.isDeadEnd }.map { $0.parentBlock })

  if liverange.nonDestroyingUsers.isEmpty {
    // Handle the corner case where the value has no use at all (beside the destroy).
    immediatelyDestroy(value: value, ifIn: liverange, &aliveDestroys, nonDeadEndRange: nonDeadEndRange)
    return aliveDestroys
  }
  // Insert new destroys at the end of the pruned liverange.
  for user in liverange.nonDestroyingUsers {
    insertDestroy(of: value, after: user, ifIn: liverange, &aliveDestroys, nonDeadEndRange: nonDeadEndRange)
  }
  // Also, we need new destroys at exit edges from the pruned liverange.
  for exitInst in liverange.prunedLiverange.exits {
    insertDestroy(of: value, before: exitInst, ifIn: liverange, &aliveDestroys, nonDeadEndRange: nonDeadEndRange)
  }
  return aliveDestroys
}

private func removeOldDestroys(of value: Value, ignoring: InstructionSet, _ context: FunctionPassContext) {
  for destroy in value.uses.users(ofType: DestroyValueInst.self) {
    if !ignoring.contains(destroy) {
      context.erase(instruction: destroy)
    }
  }
}

private func insertDestroy(of value: Value,
                           before insertionPoint: Instruction,
                           ifIn liverange: Liverange,
                           _ aliveDestroys: inout InstructionSet,
                           nonDeadEndRange: BasicBlockRange
) {
  guard liverange.isOnlyInExtendedLiverange(insertionPoint) else {
    return
  }
  if let existingDestroy = insertionPoint as? DestroyValueInst, existingDestroy.destroyedValue == value {
    aliveDestroys.insert(existingDestroy)
    return
  }
  let builder = Builder(before: insertionPoint, liverange.context)
  let newDestroy = builder.createDestroyValue(
                     operand: value,
                     isDeadEnd: !nonDeadEndRange.inclusiveRangeContains(insertionPoint.parentBlock))
  aliveDestroys.insert(newDestroy)
}

private func insertDestroy(of value: Value,
                           after insertionPoint: Instruction,
                           ifIn liverange: Liverange,
                           _ aliveDestroys: inout InstructionSet,
                           nonDeadEndRange: BasicBlockRange
) {
  if let next = insertionPoint.next {
    insertDestroy(of: value, before: next, ifIn: liverange, &aliveDestroys, nonDeadEndRange: nonDeadEndRange)
  } else {
    for succ in insertionPoint.parentBlock.successors {
      insertDestroy(of: value, before: succ.instructions.first!, ifIn: liverange,
                    &aliveDestroys, nonDeadEndRange: nonDeadEndRange)
    }
  }
}

private func immediatelyDestroy(value: Value,
                                ifIn liverange: Liverange,
                                _ aliveDestroys: inout InstructionSet,
                                nonDeadEndRange: BasicBlockRange
) {
  if let arg = value as? Argument {
    insertDestroy(of: value, before: arg.parentBlock.instructions.first!, ifIn: liverange,
                  &aliveDestroys, nonDeadEndRange: nonDeadEndRange)
  } else {
    insertDestroy(of: value, after: value.definingInstruction!, ifIn: liverange,
                  &aliveDestroys, nonDeadEndRange: nonDeadEndRange)
  }
}

private func needPreserveDebugInfo(of value: Value, _ context: FunctionPassContext) -> Bool {
  if value.parentFunction.shouldOptimize {
    // No need to preserve debug info in optimized builds.
    return false
  }
  // Check if the value is associated to a source-level variable.
  if let inst = value.definingInstruction {
    return inst.findVarDecl() != nil
  }
  if let arg = value as? Argument {
    return arg.findVarDecl() != nil
  }
  return false
}

/// Represents the "extended" liverange of a value which is the range after the last uses until the
/// final destroys of the value.
///
/// ```
///   %1 = definition          -+                         -+
///   ...                       | pruned liverange         |
///   last_use(%1)             -+   -+                     | full liverange
///   ... no uses of %1              | extended liverange  |
///   destroy_value %1              -+                    -+
/// ```
private struct Liverange {
  var nonDestroyingUsers: Stack<Instruction>
  var prunedLiverange: InstructionRange
  var fullLiverange: InstructionRange
  let context: FunctionPassContext

  init?(of value: Value, _ context: FunctionPassContext) {
    guard let users = Stack(usersOf: value, context) else {
      return nil
    }
    self.nonDestroyingUsers = users

    self.prunedLiverange = InstructionRange(for: value, context)
    prunedLiverange.insert(contentsOf: nonDestroyingUsers)

    self.fullLiverange = InstructionRange(for: value, context)
    fullLiverange.insert(contentsOf: value.users)

    self.context = context
  }

  func isOnlyInExtendedLiverange(_ instruction: Instruction) -> Bool {
    fullLiverange.inclusiveRangeContains(instruction) && !prunedLiverange.inclusiveRangeContains(instruction)
  }

  mutating func extendWithAccessScopes(of endAccesses: Stack<EndAccessInst>) {
    var changed: Bool
    // We need to do this repeatedly because if access scopes are not nested properly, an overlapping scope
    // can make a non-overlapping scope also overlapping, e.g.
    // ```
    //    %1 = begin_access   // overlapping
    //    last_use %value
    //    %2 = begin_access   // initially not overlapping, but overlapping because of scope %1
    //    end_access %1
    //    end_access %2
    //    destroy_value %value
    // ```
    repeat {
      changed = false
      for endAccess in endAccesses {
        if isOnlyInExtendedLiverange(endAccess), !isOnlyInExtendedLiverange(endAccess.beginAccess) {
          prunedLiverange.insert(endAccess)
          nonDestroyingUsers.append(endAccess)
          changed = true
        }
      }
    } while changed
  }

  mutating func deinitialize() {
    fullLiverange.deinitialize()
    prunedLiverange.deinitialize()
    nonDestroyingUsers.deinitialize()
  }
}

private extension Stack where Element == Instruction {
  init?(usersOf value: Value, _ context: FunctionPassContext) {
    var users = Stack<Instruction>(context)

    var visitor = InteriorUseWalker(definingValue: value, ignoreEscape: false, visitInnerUses: true, context) {
      if $0.instruction is DestroyValueInst,
         $0.value == value
      {
        return .continueWalk
      }
      users.append($0.instruction)
      return .continueWalk
    }
    defer { visitor.deinitialize() }

    guard visitor.visitUses() == .continueWalk else {
      users.deinitialize()
      return nil
    }
    self = users
  }
}
