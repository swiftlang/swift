//===--- CopySinking.swift -------------------------------------------------==//
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

/// Sinks copy instructions (`copy_value`, `load [copy]`) across control flow:
///
/// 1. Sinks copies over phi nodes:
///
/// ```
///   bb1:
///     %1 = copy_value %0
///     br bb3(%1)
///   bb2:
///     %2 = copy_value %0
///     br bb3(%2)
///   bb3(%3 : @owned):
///     // uses of %3
/// ```
/// ->
/// ```
///   bb1:
///     br bb3(%0)
///   bb2:
///     br bb3(%0)
///   bb3(%3 : @guaranteed):
///     %4 = copy_value %3
///     // uses of %4
/// ```
///
/// 2. Sinks copies of a phi over that phi:
///
/// ```
///   bb1:
///     %1 = copy_value %0
///     br bb3(%0, %1)
///   bb2:
///     %3 = copy_value %2
///     br bb3(%2, %3)
///   bb3(%4 : @owned, %5 : @owned):
///     // uses of %5
/// ```
/// ->
/// ```
///   bb1:
///     br bb3(%0)
///   bb2:
///     br bb3(%2)
///   bb3(%4 : @owned):
///     %6 = copy_value %4
///     // uses of %6
/// ```
///
/// 3. Sinks copies over terminator instructions (switch_enum, checked_cast_br):
///
/// ```
///   %1 = copy_value %0
///   switch_enum %1, case A: bb1, case B: bb2
///   bb1(%2 : @owned):
///     // uses of %2
///   bb2(%3 : @owned):
///     // uses of %3
/// ```
/// ->
/// ```
///   switch_enum %0, case A: bb1, case B: bb2
///   bb1(%2 : @guaranteed):
///     %4 = copy_value %2
///     // uses of %4
///   bb2(%3 : @guaranteed):
///     %5 = copy_value %3
///     // uses of %5
/// ```
///
/// 4. Also handles `load [copy]` which is replaced by `load_borrow`:
///
/// ```
///   %1 = load [copy] %addr
///   switch_enum %1, case A: bb1, case B: bb2
///   bb1(%2 : @owned):
///     // uses of %2
///   bb2(%3 : @owned):
///     // uses of %3
/// ```
/// ->
/// ```
///   %1 = load_borrow %addr
///   switch_enum %1, case A: bb1, case B: bb2
///   bb1(%2 : @guaranteed):
///     %4 = copy_value %2
///     // uses of %4
///     end_borrow %1
///   bb2(%3 : @guaranteed):
///     %5 = copy_value %3
///     // uses of %5
///     end_borrow %1
/// ```
///
/// 5. Sinks a `copy_value` to its single use if the use is in the same block:
///
/// ```
///   %1 = copy_value %0
///   %2 = alloc_stack $T
///   store %1 to [init] %2
/// ```
/// ->
/// ```
///   %2 = alloc_stack $T
///   %1 = copy_value %0
///   store %1 to [init] %2
/// ```
///
/// This does the same as the non-OSSA RetainSinking pass, which moved a `retain_value` behind
/// other instructions. It results in better LLVM code, for example LLVM's LICM can hoist loop
/// invariant calls, like witness table accessors, over instructions which are not retains.
///
/// 6. Sinks a copy into the successor block where it's actually used, if on the other control flow
///    paths the copy is only destroyed:
///
/// ```
///   %1 = load [copy] %addr        // or `copy_value` of a guaranteed value
///   cond_br %c, bb1, bb2
///   bb1:
///     destroy_value %1
///   bb2:
///     // uses of %1
/// ```
/// ->
/// ```
///   %1 = load_borrow %addr
///   cond_br %c, bb1, bb2
///   bb1:
///     end_borrow %1
///   bb2:
///     %2 = copy_value %1
///     end_borrow %1
///     // uses of %2
/// ```
///
/// Like the non-OSSA RetainSinking pass, this avoids a retain/release pair on the paths where
/// the copy is not needed.
///
/// 7. Removes a `copy_value` of an owned value which is consumed by a `checked_cast_br` if the
///    owned value is destroyed in the success block. In the failure block the original value is
///    available as the failure argument, because the cast forwards its operand unchanged on failure:
///
/// ```
///   %1 = copy_value %0                // %0 is owned
///   checked_cast_br %1, bb1, bb2
///   bb1(%2 : @owned):
///     destroy_value %0
///     // uses of %2
///   bb2(%3 : @owned):
///     destroy_value %3
///     // uses of %0
/// ```
/// ->
/// ```
///   checked_cast_br %0, bb1, bb2
///   bb1(%2 : @owned):
///     // uses of %2
///   bb2(%3 : @owned):
///     // uses of %3
/// ```
///
/// Unlike case 3, this also works for casts which don't preserve reference counts, e.g. casts
/// from `AnyObject`.
///
/// The optimization can be done if:
/// * The `copy_value` or `load [copy]` has a single use.
/// * The source operand has guaranteed ownership. In case 2 the source is a phi of the same block
///   which the copies are copies of - it can have any ownership, because the new copy is inserted
///   at the begin of the block where that phi is definitely available.
/// * The borrow scope can be extended or is already valid across the control flow.
/// * For loads: no writes to the loaded address occur before the terminator.
///
/// The optimization has several benefits:
/// * In case of phi-nodes, it reduces the number of `copy_value` instructions which is good for code size
/// * Also, if the copies are the only instructions in the predecessor block, the predecessor blocks
///   become empty which allows simplification of the control flow.
/// * In case of `switch_enum`, the copy of an enum payload is much simpler than the copy of the whole enum.
/// * It's more likely that a sunk `copy_value` can be eliminated eventually.
///
let copySinking = FunctionPass(name: "copy-sinking") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership {
    return
  }

  for block in function.blocks {
    if !context.continueWithNextSubpassRun(for: block.terminator) {
      return
    }

    for arg in block.arguments.reversed() {
      if let phi = Phi(arg), !trySinkCopiesOverPhi(phi: phi, context) {
        trySinkCopiesOfPhiOverPhi(phi: phi, context)
      }
    }
    sinkCopiesToTheirUses(in: block, context)
    sinkCopiesIntoSuccessors(of: block, context)
    switch block.terminator {
    case let switchEnum as SwitchEnumInst:
      trySinkCopiesOver(terminator: switchEnum, context)
    case let cast as CheckedCastBranchInst:
      if !tryRemoveCopyOfOwnedValue(consumedBy: cast, context) && cast.preservesReferenceCounts {
        trySinkCopiesOver(terminator: cast, context)
      }
    default:
      break
    }
  }
}

@discardableResult
private func sinkCopiesToTheirUses(in block: BasicBlock, _ context: FunctionPassContext) -> Bool {
  var changed = false
  // Iterate in reverse order, so that moving a copy doesn't affect the iteration.
  for inst in block.instructions.reversed() {
    if let copy = inst as? CopyValueInst,
       let use = copy.uses.singleUse,
       use.instruction.parentBlock == block,
       use.instruction != copy.next,
       sourceIsAlive(of: copy, until: use.instruction, context)
    {
      copy.move(before: use.instruction, context)
      changed = true
    }
  }
  return changed
}

/// Sinks `copy_value`s of guaranteed values and `load [copy]`s in `block` into the successor block
/// which dominates all their uses, if on the paths through the other successors the copy is only
/// destroyed (case 6 in the pass description).
@discardableResult
private func sinkCopiesIntoSuccessors(of block: BasicBlock, _ context: FunctionPassContext) -> Bool {
  guard block.successors.count >= 2 else {
    return false
  }
  var changed = false
  // Iterate in reverse order, so that erasing a `load [copy]` doesn't affect the iteration.
  for inst in block.instructions.reversed() {
    switch inst {
    case let copy as CopyValueInst:
      if trySinkIntoSuccessor(copy: copy, context) {
        changed = true
      }
    case let load as LoadInst where load.loadOwnership == .copy:
      if trySinkIntoSuccessor(load: load, context) {
        changed = true
      }
    default:
      break
    }
  }
  return changed
}

private func trySinkIntoSuccessor(copy: CopyValueInst, _ context: FunctionPassContext) -> Bool {
  let copyBlock = copy.parentBlock
  guard copy.fromValue.ownership == .guaranteed,
        let successorOfUses = getSuccessorWhichDominatesUses(of: copy, context),
        copy.hasDestroys(notDominatedBy: successorOfUses, context),
        extendBorrowScope(of: copy.fromValue, from: copy, to: copyBlock.terminator, context)
  else {
    return false
  }
  // `getSuccessorWhichDominatesUses` only allows `debug_value`s as uses in the copy's block.
  // Those can take the guaranteed source as operand.
  copy.uses.filter({ $0.instruction.parentBlock == copyBlock }).replaceAll(with: copy.fromValue, context)
  removeUses(of: copy, outsideOf: successorOfUses, context)

  let newCopy = Builder(atBeginOf: successorOfUses, context).createCopyValue(operand: copy.fromValue)
  copy.replace(with: newCopy, context)
  return true
}

private func trySinkIntoSuccessor(load: LoadInst, _ context: FunctionPassContext) -> Bool {
  let loadBlock = load.parentBlock
  guard let successorOfUses = getSuccessorWhichDominatesUses(of: load, context),
        load.hasDestroys(notDominatedBy: successorOfUses, context),
        !mayWriteOrEndAccess(toAddressOf: load, before: loadBlock.terminator, context)
  else {
    return false
  }
  // If the loaded address is a projection of a borrowed reference, the borrow scope of that reference
  // must enclose the new `load_borrow`.
  if let baseBorrow = load.address.beginBorrowOfAddress {
    guard extendBorrowScope(of: baseBorrow.value, from: load, to: loadBlock.terminator, context) else {
      return false
    }
  }
  let loadBorrow = Builder(before: load, context).createLoadBorrow(fromAddress: load.address)
  // `getSuccessorWhichDominatesUses` only allows `debug_value`s as uses in the load's block.
  // Those can take the guaranteed `load_borrow` as operand.
  load.uses.filter({ $0.instruction.parentBlock == loadBlock }).replaceAll(with: loadBorrow, context)
  removeUses(of: load, outsideOf: successorOfUses, context)

  for succ in loadBlock.successors {
    Builder(atBeginOf: succ, context).createEndBorrow(of: loadBorrow)
  }
  let newCopy = Builder(atBeginOf: successorOfUses, context).createCopyValue(operand: loadBorrow)
  load.replace(with: newCopy, context)
  return true
}

private func getSuccessorWhichDominatesUses(of copy: SingleValueInstruction,
                                            _ context: FunctionPassContext) -> BasicBlock?
{
  let block = copy.parentBlock
  let domTree = context.dominatorTree
  var successor: BasicBlock? = nil

  for use in copy.uses {
    let useBlock = use.instruction.parentBlock
    if useBlock == block {
      // Debug values in the copy's block will use the copy's source.
      guard use.instruction is DebugValueInst else {
        return nil
      }
      continue
    }
    // A use which is not dominated by a single-predecessor successor of the copy's block is
    // located after a control flow merge of several paths from the copy.
    guard let useSucc = block.successors.first(where: { $0.dominates(useBlock, domTree) }) else {
      return nil
    }
    if use.instruction is DestroyValueInst || use.instruction is DebugValueInst {
      continue
    }
    if let successor, successor != useSucc {
      return nil
    }
    successor = useSucc
  }
  return successor
}

private extension Value {
  func hasDestroys(notDominatedBy successor: BasicBlock, _ context: FunctionPassContext) -> Bool {
    let domTree = context.dominatorTree
    return uses.users(ofType: DestroyValueInst.self).contains { !successor.dominates($0.parentBlock, domTree) }
  }
}

/// Erases the destroys and debug_values on paths which don't go through `successor`.
private func removeUses(of copy: SingleValueInstruction,
                        outsideOf successor: BasicBlock,
                        _ context: FunctionPassContext)
{
  let domTree = context.dominatorTree
  context.erase(instructions: copy.users.filter {
    $0.parentBlock != copy.parentBlock && !successor.dominates($0.parentBlock, domTree)
  })
}

private func trySinkCopiesOverPhi(phi: Phi, _ context: FunctionPassContext) -> Bool {
  // Before address lowering, an address-only phi can't be guaranteed.
  let function = phi.value.parentFunction
  if !function.hasLoweredAddresses && !phi.value.type.isLoadable(in: function) {
    return false
  }

  for incomingOp in phi.incomingOperands {
    guard let copy = incomingOp.value as? CopyValueInst,
          copy.uses.isSingleUse,
          copy.fromValue.ownership == .guaranteed,
          sourceIsAlive(of: copy, until: incomingOp.instruction, context)
    else {
      return false
    }
  }

  for incomingOp in phi.incomingOperands {
    let copy = incomingOp.value as! CopyValueInst
    copy.replace(with: copy.fromValue, context)
  }

  phi.value.set(ownership: .guaranteed, context)
  // Create the `borrowed` instruction right away, because subsequent transformations in this pass need
  // to compute the borrow introducers of the new copy's source.
  updateBorrowedFrom(for: CollectionOfOne(phi), context)
  let borrowedFrom = phi.borrowedFrom!
  let newCopy = Builder(after: borrowedFrom, context).createCopyValue(operand: borrowedFrom)
  borrowedFrom.uses.ignore(user: newCopy).replaceAll(with: newCopy, context)

  return true
}

@discardableResult
private func trySinkCopiesOfPhiOverPhi(phi: Phi, _ context: FunctionPassContext) -> Bool {
  let block = phi.value.parentBlock
  var copiedFrom: Phi? = nil

  for incomingOp in phi.incomingOperands {
    guard let copy = incomingOp.value as? CopyValueInst,
          copy.uses.isSingleUse,
          let fromPhi = copy.fromValue.passedToPhi(via: incomingOp.instruction as! BranchInst)
    else {
      return false
    }
    if let copiedFrom {
      if fromPhi.value != copiedFrom.value {
        return false
      }
    } else {
      copiedFrom = fromPhi
    }
  }
  guard let copiedFrom else {
    return false
  }

  // Create a single copy of the copied-from phi in the phi's block and replace the phi with it.
  let newCopy: Value
  if let borrowedFrom = copiedFrom.borrowedFrom {
    newCopy = Builder(after: borrowedFrom, context).createCopyValue(operand: borrowedFrom)
  } else {
    newCopy = Builder(atBeginOf: block, context).createCopyValue(operand: copiedFrom.value)
  }
  phi.value.uses.replaceAll(with: newCopy, context)

  // Remove the phi operand from all predecessor branches and erase the now-dead incoming copies.
  erasePhiArgument(phi: phi, erasingIncomingInstructions: true, context)

  return true
}

private extension Value {
  func passedToPhi(via branch: BranchInst) -> Phi? {
    for use in uses {
      if use.instruction == branch {
        return branch.getPhi(for: use)
      }
    }
    return nil
  }
}

@discardableResult
private func trySinkCopiesOver(terminator: ForwardingInstruction & UnaryInstruction & TermInst,
                               _ context: FunctionPassContext) -> Bool
{
  if !terminator.canForwardGuaranteedAddressOnlyValues {
    return false
  }
  switch terminator.operand.value {
  case let copy as CopyValueInst:
    guard copy.uses.isSingleUse,
          copy.fromValue.ownership == .guaranteed,
          extendBorrowScope(of: copy.fromValue, from: copy, to: terminator, context)
    else {
      return false
    }

    terminator.setForwardingOwnership(to: .guaranteed, context)
    copy.replace(with: copy.fromValue, context)

    for succ in terminator.successors {
      if let arg = succ.arguments.first, arg.ownership == .owned {
        arg.set(ownership: .guaranteed, context)
        let newCopy = Builder(atBeginOf: succ, context).createCopyValue(operand: arg)
        arg.uses.ignore(user: newCopy).replaceAll(with: newCopy, context)
      }
    }
    return true

  case let load as LoadInst:
    guard load.uses.isSingleUse,
          load.loadOwnership == .copy,
          !mayWriteOrEndAccess(toAddressOf: load, before: terminator, context)
    else {
      return false
    }

    terminator.setForwardingOwnership(to: .guaranteed, context)
    let loadBorrow = Builder(before: load, context).createLoadBorrow(fromAddress: load.address)
    load.replace(with: loadBorrow, context)

    for succ in terminator.successors {
      let builder = Builder(atBeginOf: succ, context)
      if let arg = succ.arguments.first, arg.ownership == .owned {
        arg.set(ownership: .guaranteed, context)
        let newCopy = builder.createCopyValue(operand: arg)
        arg.uses.ignore(user: newCopy).replaceAll(with: newCopy, context)
      }
      builder.createEndBorrow(of: loadBorrow)
    }
    return true

  default:
    return false
  }
}

private func tryRemoveCopyOfOwnedValue(consumedBy cast: CheckedCastBranchInst,
                                       _ context: FunctionPassContext) -> Bool
{
  guard let copy = cast.operand.value as? CopyValueInst,
        copy.uses.isSingleUse,
        copy.parentBlock == cast.parentBlock,
        copy.fromValue.ownership == .owned,
        let failureArg = cast.failureBlock.arguments.first,
        let failureDestroy = failureArg.uses.singleUse?.instruction as? DestroyValueInst
  else {
    return false
  }
  let ownedValue = copy.fromValue
  let domTree = context.dominatorTree

  var successDestroy: DestroyValueInst? = nil
  var failureUses = Stack<Operand>(context)
  defer { failureUses.deinitialize() }
  var otherUseBlocks = BasicBlockSet(context)
  defer { otherUseBlocks.deinitialize() }
  otherUseBlocks.insert(cast.parentBlock)

  for use in ownedValue.uses where use.instruction != copy {
    let user = use.instruction
    if user.parentBlock == cast.parentBlock {
      // There must not be any uses between the copy and the cast.
      guard user.strictlyDominatesInBlock(copy) else {
        return false
      }
    } else if cast.successBlock.dominates(user.parentBlock, domTree) {
      // The lifetime of the owned value must end in the success block - with a single `destroy_value`.
      guard successDestroy == nil,
            let destroy = user as? DestroyValueInst,
            destroy.parentBlock == cast.successBlock
      else {
        return false
      }
      successDestroy = destroy
    } else if cast.failureBlock.dominates(user.parentBlock, domTree) {
      failureUses.append(use)
    } else {
      otherUseBlocks.insert(user.parentBlock)
    }
  }
  guard let successDestroy else {
    return false
  }

  // After the transformation the owned value is consumed by the cast. Therefore none of its other uses
  // (including the cast itself) must be reachable from the failure block, e.g. via a loop back-edge.
  var worklist = BasicBlockWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(cast.failureBlock)
  while let block = worklist.pop() {
    if otherUseBlocks.contains(block) {
      return false
    }
    worklist.pushIfNotVisited(contentsOf: block.successors)
  }

  copy.replace(with: ownedValue, context)
  context.erase(instruction: successDestroy)
  context.erase(instruction: failureDestroy)
  for use in failureUses {
    use.set(to: failureArg, context)
  }
  return true
}

/// Returns true if the source of `copy` is alive in the range from `copy` to `endInstruction`.
/// The source's lifetime may end at `endInstruction`.
private func sourceIsAlive(of copy: CopyValueInst,
                           until endInstruction: Instruction,
                           _ context: FunctionPassContext) -> Bool
{
  var liveRange = InstructionRange(begin: copy, ends: [endInstruction], context)
  defer { liveRange.deinitialize() }

  return liveRange.isFullyContainedIn(scopeOf: copy.fromValue, context)
}

private func extendBorrowScope(of value: Value,
                               from copy: SingleValueInstruction,
                               to terminator: TermInst,
                               _ context: FunctionPassContext) -> Bool
{
  var liveRange = InstructionRange(begin: copy, context)
  defer { liveRange.deinitialize() }

  liveRange.insert(contentsOf: terminator.successors.map { $0.instructions.first! })

  return extendBorrowScope(of: value, toOverlap: liveRange, context)
}

/// Returns true if the loaded memory may be written, or its access scope ends, before `endInstruction`. Either way,
/// a `load_borrow` couldn't replace the load.
private func mayWriteOrEndAccess(toAddressOf load: LoadInst,
                                 before endInstruction: Instruction,
                                 _ context: FunctionPassContext) -> Bool
{
  let aliasAnalysis = context.aliasAnalysis
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushPredecessors(of: endInstruction, ignoring: load)

  while let inst = worklist.pop() {
    if inst.mayWrite(toAddress: load.address, aliasAnalysis) || inst.endsAccessScope(of: load) {
      return true
    }
    worklist.pushPredecessors(of: inst, ignoring: load)
  }
  return false
}
