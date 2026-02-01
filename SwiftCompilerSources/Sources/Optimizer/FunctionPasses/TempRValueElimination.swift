//===--- TempRValueElimination.swift ---------------------------------------==//
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

import AST
import SIL

/// Eliminates copies to `alloc_stack` "temporaries" (r-values).
///
/// ```
///   %temp = alloc_stack $T
///   copy_addr %src to [init] %temp    -+
///   ...                                |
///   %l = load %temp                    | no writes to %src or %temp
///   ...                                |
///   destroy_addr %temp                -+
///   dealloc_stack %temp
/// ```
/// ->
/// ```
///   %l = load %src
/// ```
///
/// This differs from the copy forwarding algorithm because it handles copy source and dest lifetimes
/// that are unavoidably overlapping. Instead, it finds cases in which it is easy to determine that
/// the source is unmodified during the copy destination's lifetime. Thus, the destination can be viewed
/// as a short-lived "rvalue".
///
let tempRValueElimination = FunctionPass(name: "temp-rvalue-elimination") {
  (function: Function, context: FunctionPassContext) in
  removeTempRValues(in: function, keepDebugInfo: false, context)
}

let mandatoryTempRValueElimination = FunctionPass(name: "mandatory-temp-rvalue-elimination") {
  (function: Function, context: FunctionPassContext) in
  removeTempRValues(in: function, keepDebugInfo: true, context)
}

private func removeTempRValues(in function: Function, keepDebugInfo: Bool, _ context: FunctionPassContext) {
  for inst in function.instructions {
    switch inst {
    case let copy as CopyAddrInst:
      if copy.source == copy.destination {
        // Remove identity copies which may have been created by an earlier iteration, where another `copy_addr`
        // copied the `alloc_stack` back to the source location.
        context.erase(instruction: copy)
      } else {
        tryEliminate(copy: copy, keepDebugInfo: keepDebugInfo, context)
      }
    case let store as StoreInst:
      // Also handle `load`-`store` pairs which are basically the same thing as a `copy_addr`.
      if let load = store.source as? LoadInst, load.uses.isSingleUse, load.parentBlock == store.parentBlock {
        tryEliminate(copy: store, keepDebugInfo: keepDebugInfo, context)
      }
    default:
      break
    }
  }
}

private func tryEliminate(copy: CopyLikeInstruction, keepDebugInfo: Bool, _ context: FunctionPassContext) {

  guard let (allocStack, lastUseOfAllocStack) =
          getRemovableAllocStackDestination(of: copy, keepDebugInfo: keepDebugInfo, context),
        let needHoistDestroys = needHoistDestroys(of: allocStack, after: lastUseOfAllocStack, copy: copy, context)
  else {
    return
  }

  guard extendAccessScopes(beyond: lastUseOfAllocStack, copy: copy, context) else {
    return
  }

  if needHoistDestroys {
    // Hoist destroy_addrs after the last use, because between the last use and the original destroy
    // the source is modified.
    hoistDestroy(of: allocStack, after: lastUseOfAllocStack, context)
  } else if !copy.isTakeOfSource {
    removeDestroys(of: allocStack, context)
  }

  allocStack.uses.ignore(usersOfType: DeallocStackInst.self).replaceAll(with: copy.sourceAddress, context)

  if keepDebugInfo {
    Builder(before: copy, context).createDebugStep()
  }
  context.erase(instructionIncludingAllUsers: copy.loadingInstruction)
  context.erase(instructionIncludingAllUsers: allocStack)
}

/// Checks if the `copy` is copying into an `alloc_stack` which is removable:
/// ```
///   %allocStack = alloc_stack $T
///   copy_addr %src to [init] %allocStack
///   ...
///   %lastUseOfAllocStack = load %allocStack
/// ```
private func getRemovableAllocStackDestination(
  of copy: CopyLikeInstruction, keepDebugInfo: Bool, _ context: FunctionPassContext
) -> (allocStack: AllocStackInst, lastUseOfAllocStack: Instruction)? {
  guard copy.isInitializationOfDestination,
        let allocStack = copy.destinationAddress as? AllocStackInst
  else {
    return nil
  }

  if keepDebugInfo {
    if allocStack.isFromVarDecl || allocStack.isLexical {
      return nil
    }
  } else {
    // If the `allocStack` is lexical we can eliminate it if the source of the copy is lexical and
    // it is live for longer than the `allocStack`.
    if allocStack.isLexical && !copy.sourceAddress.accessBase.storageIsLexical {
      return nil
    }
  }

  var allocStackUses = UseCollector(copy: copy, context)
  defer { allocStackUses.deinitialize() }

  // Scan all uses of the `allocStack` to verify only the `copy` is writing to it and that all uses
  // (except `destroy_addr` and `dealloc_stack`) are in the same basic block.
  guard allocStackUses.collectUses(of: allocStack) else {
    return nil
  }

  // Check that there are no uses of the `allocStack` that precede the copyInst. This can happen with projections.
  // TODO: We can enable this case if we clone the projections at "load" uses.
  if allocStack.hasUses(before: copy, context) {
    return nil
  }

  // Check if the source is modified within the lifetime of the 'alloc_stack'.
  guard let lastUseOfAllocStack = getLastUseWhileSourceIsNotModified(of: copy, uses: allocStackUses.uses, context) else {
    return nil
  }

  // Bail if in non-OSSA the `allocStack` is destroyed in a non-obvious way, e.g. by
  // ```
  //   %x = load %allocStack   // looks like a load, but is a `load [take]`
  //   strong_release %x
  // ```
  guard copy.parentFunction.hasOwnership ||
        allocStack.isDestroyedOnAllPaths(context) ||
        // We can easily remove a dead alloc_stack
        allocStack.uses.ignore(user: copy).ignore(usersOfType: DeallocStackInst.self).isEmpty
  else {
    return nil
  }

  return (allocStack, lastUseOfAllocStack)
}

/// Returns true if the final `destroy_addr`s need to be hoisted to the last use of the `allocStack`.
/// This is required if the copy-source is re-initialized inbetween, e.g.
/// ```
///   copy_addr %source, %allocStack
///   ...
///   last_use(%allocStack)
///   ...                                <- the destroy_addr needs to be moved here
///   store %newValue to [init] %source
///   ...
///   destroy_addr %allocStack
/// ```
/// Returns nil if hoisting is needed but not possible.
private func needHoistDestroys(of allocStack: AllocStackInst,
                               after lastUseOfAllocStack: Instruction,
                               copy: CopyLikeInstruction,
                               _ context: FunctionPassContext
) -> Bool? {
  guard copy.isTakeOfSource else {
    return false
  }
  if lastUseOfAllocStack is DestroyAddrInst {
    assert(!copy.parentFunction.hasOwnership, "should not treat destroy_addr as uses in OSSA")
    return false
  }

  // We cannot insert the destroy of `copy.source` after `lastUseOfAllocStack` if `copy.source` is
  // re-initialized by exactly this instruction.
  // This is a corner case, but can happen if `lastUseOfAllocStack` is a `copy_addr`. Example:
  // ```
  //   copy_addr [take] %src to [init] %allocStack   // `copy`
  //   copy_addr [take] %allocStack to [init] %src   // `lastUseOfAllocStack`
  // ```
  if lastUseOfAllocStack != copy, lastUseOfAllocStack.mayWrite(toAddress: copy.sourceAddress, context.aliasAnalysis) {
    return nil
  }

  if mayWrite(to: copy.sourceAddress, after: lastUseOfAllocStack, beforeDestroysOf: allocStack, context) {
    if hasDestroyBarrier(after: lastUseOfAllocStack, beforeDestroysOf: allocStack, context) {
      return nil
    }
    return true
  }
  return false
}

private func mayWrite(to source: Value,
                      after lastUse: Instruction,
                      beforeDestroysOf allocStack: AllocStackInst,
                      _ context: FunctionPassContext
) -> Bool {
  return visitInstructions(after: lastUse, beforeDestroysOf: allocStack, context) { inst in
    inst.mayWrite(toAddress: source, context.aliasAnalysis)
  }
}

private func hasDestroyBarrier(after lastUse: Instruction,
                              beforeDestroysOf allocStack: AllocStackInst,
                               _ context: FunctionPassContext
) -> Bool {
  return visitInstructions(after: lastUse, beforeDestroysOf: allocStack, context) { inst in
    inst.isBarrierForDestroy(of: allocStack.type, context)
  }
}

private func visitInstructions(after lastUse: Instruction,
                               beforeDestroysOf allocStack: AllocStackInst,
                               _ context: FunctionPassContext,
                               _ predicate: (Instruction) -> Bool
) -> Bool {
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  for destroy in allocStack.uses.users(ofType: DestroyAddrInst.self) {
    worklist.pushPredecessors(of: destroy, ignoring: lastUse)
  }

  while let inst = worklist.pop() {
    if predicate(inst) {
      return true
    }
    worklist.pushPredecessors(of: inst, ignoring: lastUse)
  }
  return false
}

// We need to hoist the destroy of the stack location up to its last use because the source of the copy
// could be re-initialized between the last use and the destroy.
private func hoistDestroy(of allocStack: AllocStackInst, after lastUse: Instruction, _ context: FunctionPassContext) {
  // Check if the last use already takes the stack location,
  switch lastUse {
  case let cai as CopyAddrInst where cai.source == allocStack && cai.isTakeOfSource:
    return
  case let li as LoadInst where li.loadOwnership == .take:
    assert(li.address == allocStack, "load must be not take a projected address")
    return
  case let apply as ApplyInst where apply.consumes(address: allocStack):
    if allocStack.uses.contains(where: { $0.instruction == apply && apply.convention(of: $0)!.isGuaranteed }) {
      return
    }
  default:
    break
  }
  if allocStack.uses.users(ofType: DestroyAddrInst.self).isEmpty {
    // The stack location is not destroyed at all! This can happen if it contains a non-copyable
    // value which has only trivial fields.
    return
  }

  // Note that there can be multiple destroy_addr because they don't need to be in the same block
  // as the alloc_stack, e.g.
  //   %1 = alloc_stack
  //   cond_br %c, bb1, bb2
  // bb1:
  //   destroy_addr %1
  // bb2:
  //   destroy_addr %1
  context.erase(instructions: allocStack.uses.users(ofType: DestroyAddrInst.self))

  Builder.insert(after: lastUse, context) { builder in
    builder.createDestroyAddr(address: allocStack)
  }
}

private func removeDestroys(of allocStack: AllocStackInst, _ context: FunctionPassContext) {
  for use in allocStack.uses {
    switch use.instruction {
    case is DestroyAddrInst:
      context.erase(instruction: use.instruction)
    case let cai as CopyAddrInst where cai.isTakeOfSource:
      assert(cai.source == allocStack, "partial takes of the stack location are not supported")
      cai.set(isTakeOfSource: false, context)
    case let load as LoadInst where load.loadOwnership == .take:
      assert(load.address == allocStack, "partial takes of the stack location are not supported")
      load.set(ownership: .copy, context)
    default:
      // Note that no operations other than the cases above can destroy the `allocStack` (we checked
      // this in the `UseCollector`).
      break
    }
  }
}


private extension AllocStackInst {
  func hasUses(before instruction: Instruction, _ context: FunctionPassContext) -> Bool {
    var useSet = InstructionSet(context)
    defer { useSet.deinitialize() }

    useSet.insert(contentsOf: self.users)
    for inst in InstructionList(first: self) {
      if inst == instruction {
        return false
      }
      if useSet.contains(inst) {
        return true
      }
    }
    return false
  }

  /// In non-OSSA, check if the `alloc_stack` destroyed in an obvious way and not e.g. implicitly by
  /// ```
  ///   %x = load %allocStack   // looks like a load, but is a `load [take]`
  ///   strong_release %x
  /// ```
  func isDestroyedOnAllPaths(_ context: FunctionPassContext) -> Bool {
    var liferange = InstructionRange(begin: self, context)
    defer { liferange.deinitialize() }

    liferange.insert(contentsOf: uses.ignore(usersOfType: DeallocStackInst.self).lazy.map { $0.instruction })

    for use in uses {
      switch use.instruction {
      case is DeallocStackInst, is DestroyAddrInst:
        break
      case let c as CopyAddrInst where c.sourceOperand == use && c.isTakeOfSource:
        break
      default:
        if !liferange.contains(use.instruction) {
          // A non-destroying instruction is at the end of the liferange -> we are missing a "real" destroy.
          return false
        }
      }
    }
    return true
  }
}

private extension ApplySite {
  func consumes(address: Value) -> Bool {
    argumentOperands.contains { argOp in
      argOp.value == address && convention(of: argOp)!.isConsumed
    }
  }
}

/// Tries to move an `end_access` down to extend the access scope over all uses of the `alloc_stack`.
/// For example:
/// ```
///   %a = begin_access %src
///   copy_addr %a to [init] %temp : $*T
///   end_access %a
///   use %temp
/// ```
/// We must not replace %temp with %a after the `end_access`. Instead we try to move the `end_access`
/// after the last use.
private func extendAccessScopes(beyond lastUse: Instruction, copy: CopyLikeInstruction,
                                _ context: FunctionPassContext) -> Bool
{
  var endAccessToMove: EndAccessInst? = nil

  for inst in InstructionList(first: copy.next) {
    if let endAccess = inst as? EndAccessInst {
      // To keep things simple, we can just move a single `end_access`. Also, we cannot move an
      // `end_access` over a (non-aliasing) other `end_access`.
      if endAccessToMove != nil {
        return false
      }
      if context.aliasAnalysis.mayAlias(copy.sourceAddress, endAccess.beginAccess.address),
         // There cannot be any aliasing modifying accesses within the liverange of the `alloc_stack`,
         // because we would have cought this in `getLastUseWhileSourceIsNotModified`.
         // But there are cases where `aliasAnalysis.mayAlias` is less precise than `Instruction.mayWrite`.
         // Therefore, just ignore any non-read accesses.
         endAccess.beginAccess.accessKind == .read
      {
        // We cannot move instructions beyond the block's terminator.
        if lastUse is TermInst {
          return false
        }
        endAccessToMove = endAccess
      }
    } else if let endAccessToMove {
      // We cannot move an `end_access` over a `begin_access`. This would destroy the proper nesting of accesses.
      if inst is BeginAccessInst || inst is BeginUnpairedAccessInst {
        return false
      }
      // Don't extend a read-access scope over a (potential) write.
      // Note that `inst` can be a function call containing other access scopes. But doing the `inst.mayWrite`
      // check, we know that the function can only contain read accesses (to the same memory location).
      // So it's fine to move `endAccessToMove` even over such a function call.
      if inst.mayWrite(toAddress: endAccessToMove.beginAccess.address, context.aliasAnalysis) {
        return false
      }
    }
    if inst == lastUse {
      break
    }
  }
  if let endAccessToMove {
    endAccessToMove.move(before: lastUse.next!, context)
  }

  return true
}

/// Checks if the source of `copy` is not modified within the `alloc_stack`'s lifetime, i.e. is not modified
/// before the last use of `uses`.
///
/// If there are no source modifications with the lifetime, returns the last user. Otherwise returns nil.
///
/// Unfortunately, we cannot simply use the destroy points as the lifetime end, because they can be in a
/// different basic block. Instead we look for the last non-destroy, non-dealloc use.
private func getLastUseWhileSourceIsNotModified(of copy: CopyLikeInstruction,
                                                uses: InstructionSetWithCount,
                                                _ context: FunctionPassContext) -> Instruction?
{
  var numUsesFound = 0
  if copy == copy.loadingInstruction {
    // As we are starting the iteration at the next instruction after the copy's load instruction
    // we pretend to having seen the copy instruction already.
    numUsesFound += 1
  }
  if uses.count == numUsesFound {
    return copy
  }
  let aliasAnalysis = context.aliasAnalysis

  // We already checked that the useful lifetime of the `alloc_stack` ends in the same block as the `copy`.
  // Therefore we can limit our search to the instructions of this block.
  for inst in InstructionList(first: copy.loadingInstruction.next) {
    if uses.contains(inst) {
      numUsesFound += 1
    }

    // If this is the last use of the `alloc_stack` we are okay. After this point, modifications to the source
    // don't matter anymore.
    // Note that we are assuming here that if an instruction reads and writes to the source at the same time
    // (like a `copy_addr` could do), the write takes effect after the read.
    if numUsesFound == uses.count {
      // Function calls are an exception: in a called function a potential modification of `copy.source`
      // could occur _before_ the read of the `alloc_stack`.
      switch inst {
      case is FullApplySite, is YieldInst:
        if inst.mayWrite(toAddress: copy.sourceAddress, aliasAnalysis) {
          return nil
        }
        return inst
      default:
        return inst
      }
    }

    if inst.mayWrite(toAddress: copy.sourceAddress, aliasAnalysis) {
      return nil
    }
  }
  // For some reason, not all normal uses have been seen between the copy and the end of the initialization
  // block. We should never reach here.
  return nil
}

/// Collects all uses of the `alloc_stack`.
private struct UseCollector : AddressDefUseWalker {
  private(set) var uses: InstructionSetWithCount
  private let copy: CopyLikeInstruction

  init(copy: CopyLikeInstruction, _ context: FunctionPassContext) {
    self.uses = InstructionSetWithCount(context)
    self.copy = copy
  }

  mutating func collectUses(of allocStack: AllocStackInst) -> Bool {
    for use in allocStack.uses {
      switch use.instruction {
      case copy:
        uses.insert(copy)
      case is DeallocStackInst:
        // Deallocations are allowed to be in a different block.
        break
      case let destroyAddr as DestroyAddrInst:
        if !destroyAddr.parentFunction.hasOwnership && copy.isTakeOfSource {
          // In non-OSSA mode, for the purpose of inserting the destroy of `copy.source`, we have to be
          // conservative and assume that the lifetime of `allocStack` goes beyond it's last use - until
          // the final `destroy_addr`. Otherwise we would risk inserting the destroy too early. Therefore we
          // treat the `destroy_addr` as any other use of `allocStack` and require it to be in the same block.
          if destroyAddr.parentBlock != copy.parentBlock {
            return false
          }
          uses.insert(destroyAddr)
        }
      default:
        if walkDown(address: use, path: UnusedWalkingPath()) == .abortWalk {
          return false
        }
      }
    }
    return true
  }

  public mutating func walkDown(address operand: Operand, path: UnusedWalkingPath) -> WalkResult {
    if operand.instruction.parentBlock != copy.parentBlock {
      // All normal uses (except destroys and `dealloc_stack`) must be in the initialization block.
      return .abortWalk
    }
    switch operand.instruction {
    case let openExistential as OpenExistentialAddrInst:
      if !openExistential.isImmutable {
        return.abortWalk
      }
    case let takeEnum as UncheckedTakeEnumDataAddrInst:
      // In certain cases, `unchecked_take_enum_data_addr` invalidates the underlying memory. Therefore,
      // by default` we can not look through it... but this is not true in the case of `Optional`.
      // This is an important case for us to handle, so handle it here.
      if !takeEnum.enum.type.isOptional {
        return .abortWalk
      }
    case let beginAccess as BeginAccessInst:
      if beginAccess.accessKind != .read {
        return .abortWalk
      }
      // We don't have to walk down the `beginAccess` result, because the access kind "read" already
      // guarantees that there are no writes to the `beginAccess` result address. But we have to register
      // the `end_access`es as uses to correctly mark the end-of-lifetime of the `alloc_stack`.
      // ```
      //   %addr = begin_access [read]
      //      ... // there can be no writes to %addr here
      //   end_access %addr   // <- This is where the use actually ends.
      // ```
      for endAccess in beginAccess.endAccessInstructions {
        if endAccess.parentBlock != copy.parentBlock {
          return .abortWalk
        }
        uses.insert(endAccess)
      }
      return .continueWalk
    default:
      break
    }
    return walkDownDefault(address: operand, path: path)
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    if address.isTypeDependent {
      return .continueWalk
    }

    // Only allow uses that cannot destroy their operand. We need to be sure that replacing all the uses
    // with the copy source doesn't destroy the source.
    switch address.instruction {
    case let beginApply as BeginApplyInst:
      // Extend the lifetime of the `alloc_stack` to the 'end_apply'/'abort_apply'.
      for tokenUse in beginApply.token.uses {
        if tokenUse.instruction.parentBlock != copy.parentBlock {
          return .abortWalk
        }
        uses.insert(tokenUse.instruction)
      }
      return visitApply(address: address, apply: beginApply)

    case let partialApply as PartialApplyInst:
      if !partialApply.isOnStack {
        return .abortWalk
      }
      return visitApply(address: address, apply: partialApply)

    case let apply as ApplySite:
      // Remaining applies: `apply` and `try_apply`
      return visitApply(address: address, apply: apply)

    case let yield as YieldInst:
      if !yield.convention(of: address).isGuaranteed {
        return .abortWalk
      }
      uses.insert(yield)
      return .continueWalk

    case let addrCast as UncheckedAddrCastInst:
      return walkDownUses(ofAddress: addrCast, path: UnusedWalkingPath())

    case let load as LoadInst:
      if load.loadOwnership == .take,
          // Only accept `load [take]` if it takes the whole `alloc_stack`. A `load [take]` from
          // a projection would destroy only a part of the `alloc_stack` and we don't handle this.
          load.address != copy.destinationAddress
      {
        return .abortWalk
      }
      uses.insert(load)
      return .continueWalk

    case let loadBorrow as LoadBorrowInst:
      uses.insert(loadBorrow)
      for end in loadBorrow.uses.endingLifetime.users {
        if end.parentBlock != copy.parentBlock || end is BranchInst {
          return .abortWalk
        }
        uses.insert(end)
      }
      return .continueWalk

    case let fixLifetime as FixLifetimeInst:
      // After removing the `alloc_stack` the `fix_lifetime` will apply for the `copy.source`.
      uses.insert(fixLifetime)
      return .continueWalk

    case let copyFromStack as CopyAddrInst:
      if copyFromStack.destinationOperand == address {
        return .abortWalk
      }
      // As with `load [take]`, only accept `copy_addr [take]` if it takes the whole `alloc_stack`.
      if copyFromStack.isTakeOfSource && copyFromStack.source != copy.destinationAddress {
        return .abortWalk
      }
      uses.insert(copyFromStack)
      return .continueWalk

    case is DebugValueInst:
      return .continueWalk

    default:
      return .abortWalk
    }
  }

  private mutating func visitApply(address: Operand, apply: ApplySite) -> WalkResult {
    let argConvention = apply.convention(of: address)!
    guard argConvention.isGuaranteed ||
          // Only accept consuming-in arguments if it consumes the whole `alloc_stack`. A consume from
          // a projection would destroy only a part of the `alloc_stack` and we don't handle this.
          (argConvention == .indirectIn && (copy.isTakeOfSource && address.value == copy.destinationAddress))
    else {
      return .abortWalk
    }
    uses.insert(apply)
    return .continueWalk
  }

  public mutating func unmatchedPath(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    return .abortWalk
  }

  mutating func deinitialize() {
    uses.deinitialize()
  }
}
