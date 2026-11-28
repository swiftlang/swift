//===--- ForwardChainSinking.swift -----------------------------------------==//
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

import SIL

/// Sinks the "forward chain" of a `copy_value` behind the liverange of the copy's source.
///
/// The forward chain is the copy's single consuming use - typically an ownership forwarding
/// instruction like `struct`, `enum` or `unchecked_ref_cast` - together with all its transitive users
/// which are located within the liverange of the copy's source:
///
/// ```
///   %1 = copy_value %0
///   %2 = struct $S (%1)          // forward chain: consumes %1 within the liverange of %0
///   %3 = enum $E, #E.a, %2       // forward chain
///   %4 = apply %f(%0)            // last use of %0: the end of its liverange
///   destroy_value %0
///   return %3
/// ```
/// ->
/// ```
///   %1 = copy_value %0
///   %4 = apply %f(%0)
///   %2 = struct $S (%1)          // now behind the liverange of %0
///   %3 = enum $E, #E.a, %2
///   destroy_value %0
///   return %3
/// ```
///
/// This unblocks copy-propagation: it cannot remove a copy as long as the copy is consumed within
/// the liverange of its source, because replacing the copy with its source would end the source's
/// lifetime before its last use. After sinking, copy-propagation can remove the copy:
///
/// ```
///   %4 = apply %f(%0)
///   %2 = struct $S (%0)
///   %3 = enum $E, #E.a, %2
///   return %3
/// ```
///
/// The relevant liverange of the source begins at the copy and includes all uses of the source behind
/// the copy - except destroys - as well as the ends of scopes of the source, e.g. the `end_borrow` of
/// a `begin_borrow` of the source, even if the scope begins before the copy.
///
/// The forward chain is only sunk if
/// * the liverange has a single end and no exits, i.e. the insertion point behind the end
///   post-dominates the whole liverange, and the source is not consumed within the liverange,
/// * none of the instructions in the chain has side effects (including traps), reads or writes memory,
///   is a terminator or allocates stack memory,
/// * all operands of the chain's instructions are still available at the insertion point, e.g. the
///   base of a `mark_dependence` is not destroyed and its borrow scope is not ended before.
///
let forwardChainSinking = FunctionPass(name: "forward-chain-sinking") {
  (function: Function, context: FunctionPassContext) in

  // Collect the copies before doing any transformation: sinking moves instructions to a different
  // place in the instruction list, which would invalidate an ongoing iteration over the function's
  // instructions.
  var copies = Stack<CopyValueInst>(context)
  defer { copies.deinitialize() }

  for inst in function.instructions {
    if let copy = inst as? CopyValueInst {
      copies.append(copy)
    }
  }

  for copy in copies {
    sinkForwardingUses(of: copy, context)
  }
}

/// Sinks the chain of instructions which is derived from the consuming use of `copy` behind the
/// liverange of the copy's source value.
private func sinkForwardingUses(of copy: CopyValueInst, _ context: FunctionPassContext) {
  guard var sourceLiverange = computeLiveRangeOfSource(of: copy, context) else {
    return
  }
  defer { sourceLiverange.deinitialize() }

  // Instructions are moved behind the single end of the liverange. Requiring that the liverange
  // doesn't have any exits guarantees that this insertion point post-dominates the whole liverange,
  // i.e. that it is reached from all the original locations of the moved instructions.
  guard sourceLiverange.exits.isEmpty,
        let insertionPoint = sourceLiverange.ends.singleElement?.next
  else {
    return
  }

  guard var toMove = collectInstructionsToMove(usersOf: copy, within: sourceLiverange,
                                               insertingBefore: insertionPoint, context)
  else {
    return
  }
  defer { toMove.deinitialize() }

  for inst in toMove {
    inst.moveRecursively(before: insertionPoint, &toMove, context)
  }
}

/// Computes the liverange of the copy's source value, or returns nil if the source's liverange is not
/// a candidate for sinking the copy's users behind it.
///
/// Only the part of the liverange which starts at the copy is relevant, because instructions are only
/// moved forward and they are always located behind the copy. Therefore the range begins at the copy
/// and not at the definition of the source. Otherwise control flow between the source's definition and
/// the copy would result in exits of the range.
///
/// Destroys of the source are not part of the liverange: they are located behind the last "real" use
/// and it's exactly the point behind that last use where instructions are moved to.
/// ```
///   %1 = copy_value %0
///   %2 = apply %f(%0)    // the end of the liverange
///                        // <- instructions are moved to here
///   destroy_value %0
/// ```
private func computeLiveRangeOfSource(of copy: CopyValueInst, _ context: FunctionPassContext) -> InstructionRange? {
  let domTree = context.dominatorTree

  // All instructions which are inserted into the range are dominated by the copy (see below).
  var sourceLiverange = InstructionRange(begin: copy, context)
  var liverangeIsEmpty = true

  var worklist = OperandWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(contentsOf: copy.fromValue.uses)

  while let use = worklist.pop() {
    let user = use.instruction
    if user is DestroyValueInst {
      continue
    }
    if let scope = user as? ScopedInstruction {
      // The source is used until the end of the scope, e.g. for a `begin_borrow` of the source.
      worklist.pushIfNotVisited(contentsOf: scope.scopeEndingOperands)
    }
    if user.dominates(copy, domTree) {
      // Uses before the copy are irrelevant: instructions are only moved forward, and they are
      // always located behind the copy. But a scope which begins before the copy can still end
      // behind it. That's why the scope ending operands are pushed to the worklist above.
      continue
    }
    if // Scope ending operands, e.g. of an `end_borrow`, end the lifetime of the scope's result,
       // but not the lifetime of the source.
       (use.endsLifetime && use.value == copy.fromValue) ||
       // There is no insertion point behind a terminator instruction.
       user is TermInst ||
       // Bail if there is a use which is not dominated by the copy. Otherwise the end of the
       // liverange - and therefore the insertion point - would not be dominated by the copy.
       !copy.dominates(user, domTree)
    {
      sourceLiverange.deinitialize()
      return nil
    }
    liverangeIsEmpty = false
    sourceLiverange.insert(user)
  }

  if liverangeIsEmpty {
    // Nothing to sink behind: the source is only used by the copy (and by destroys).
    sourceLiverange.deinitialize()
    return nil
  }
  return sourceLiverange
}

/// Collects the instructions which need to be moved to make the copy's lifetime ending use available
/// behind `liverange`: the transitive users of that use, as far as they are within the liverange.
///
/// Returns nil if it's not possible to move all those instructions, e.g. if one of them has side
/// effects.
private func collectInstructionsToMove(usersOf copy: CopyValueInst,
                                       within liverange: InstructionRange,
                                       insertingBefore insertionPoint: Instruction,
                                       _ context: FunctionPassContext
) -> IterableInstructionSet? {
  // With multiple consumes of the copy there is no single chain of instructions which could be moved.
  guard let singleEndLifetimeUse = copy.uses.endingLifetime.singleUse else {
    return nil
  }

  var transitiveUsers = InstructionWorklist(context)
  defer { transitiveUsers.deinitialize() }

  var toMove = IterableInstructionSet(context)

  transitiveUsers.pushIfNotVisited(singleEndLifetimeUse.instruction)

  while let copyUser = transitiveUsers.pop() {
    if liverange.isEnd(copyUser) {
      // The end of the source's liverange uses a value of the chain, e.g.
      // ```
      //   %1 = copy_value %0
      //   %2 = struct $S (%1)
      //   %3 = mark_dependence %2 on %0   // the end of the liverange of %0
      // ```
      // The end cannot be moved behind itself. And moving only the chain's instructions which
      // precede the end would move them behind their user.
      toMove.deinitialize()
      return nil
    }
    if !liverange.contains(copyUser) {
      // Users behind the liverange don't need to be moved. Note that all users of a moved instruction
      // are either within the liverange - and therefore moved as well - or behind the insertion point
      // (the end of the liverange itself is handled above).
      continue
    }
    if copyUser.mayReadOrWriteMemory || copyUser.hasUnspecifiedSideEffects || copyUser is TermInst ||
       copyUser.isAllocatingStack
    {
      // Moving side effects (this includes trapping instructions) behind other side effects would
      // change the program's behavior. And a terminator instruction cannot be moved at all.
      // Stack allocations, e.g. an `alloc_stack` which is a type-dependent user of an
      // `open_existential_ref`, cannot be moved because this could violate stack discipline.
      toMove.deinitialize()
      return nil
    }
    toMove.insert(copyUser)
    for result in copyUser.results {
      transitiveUsers.pushIfNotVisited(contentsOf: result.users)
    }
  }

  if !allOperandsAvailable(for: toMove, of: copy, at: insertionPoint, context) {
    toMove.deinitialize()
    return nil
  }
  return toMove
}

/// Returns true if all operands of the instructions in `toMove` are available at `insertionPoint`,
/// i.e. if it's legal to move those instructions there.
///
/// For example, the `mark_dependence` cannot be moved because its base operand is destroyed within the
/// liverange of `%0`:
/// ```
///   %1 = copy_value %0
///   %2 = mark_dependence %1 on %other
///   destroy_value %other        // %other is not available behind this destroy
///   %3 = apply %f(%0)           // the end of the liverange of %0
///                               // <- the insertion point
/// ```
///
/// This must be done after collecting all instructions, because an operand's defining instruction is
/// not necessarily visited before its user during the use-def walk.
private func allOperandsAvailable(for toMove: IterableInstructionSet,
                                  of copy: CopyValueInst,
                                  at insertionPoint: Instruction,
                                  _ context: FunctionPassContext) -> Bool {
  // All instructions in `toMove` are dominated by the copy. Therefore this range contains all paths from
  // their original locations to the insertion point.
  // Note that the insertion point - and not the end of the source's liverange - must be the end of the
  // range: the end of the source's liverange can end the lifetime of an operand, e.g. if it's the
  // `end_borrow` of a borrow scope which is the base of a moved `mark_dependence`.
  var range = InstructionRange(begin: copy, ends: [insertionPoint], context)
  defer { range.deinitialize() }

  for inst in toMove {
    for operand in inst.operands {
      let value = operand.value
      if value == copy {
        // The copy dominates the end of the source's liverange and its single lifetime ending use is
        // moved to the insertion point, too. Therefore the copy is available at the insertion point.
        continue
      }
      if let definingInst = value.definingInstruction, toMove.contains(definingInst) {
        // The operand is defined by an instruction which is moved to the insertion point, as well.
        // `moveRecursively` makes sure that it's moved before its users.
        continue
      }
      if !value.isAvailable(at: insertionPoint, alongPathsIn: range, context) {
        return false
      }
    }
  }
  return true
}

private extension Value {
  /// Returns true if this value is defined before `insertionPoint` and if it's still valid at that point,
  /// i.e. it's neither destroyed nor is its borrow scope ended in `range`, which contains all paths from
  /// the original locations of the moved instructions to the insertion point.
  func isAvailable(at insertionPoint: Instruction,
                   alongPathsIn range: InstructionRange,
                   _ context: FunctionPassContext) -> Bool
  {
    if !strictlyDominates(instruction: insertionPoint, context.dominatorTree) {
      return false
    }
    switch ownership {
    case .owned, .guaranteed:
      return range.isFullyContainedIn(scopeOf: self, context)
    case .none:
      // Trivial values don't have a lifetime at all. But the validity of an address depends on the
      // enclosing allocation- and access-scopes, which are not modeled by lifetime ending uses.
      return !type.isAddress
    case .unowned:
      // The validity of an unowned value depends on the lifetime of the underlying object, which is
      // not modeled by lifetime ending uses.
      return false
    }
  }
}

private extension Instruction {
  /// Moves this instruction before `insertionPoint` and removes it from `toMove`.
  ///
  /// Operands which are contained in `toMove` are moved first, so that all instructions end up in the
  /// right order at the insertion point.
  func moveRecursively(before insertionPoint: Instruction,
                       _ toMove: inout IterableInstructionSet,
                       _ context: FunctionPassContext)
  {
    if !toMove.contains(self) {
      // Either this instruction is not moved at all or it's already moved.
      return
    }
    toMove.erase(self)

    for op in operands {
      if let opInst = op.value.definingInstruction {
        opInst.moveRecursively(before: insertionPoint, &toMove, context)
      }
    }
    move(before: insertionPoint, context)
  }
}
