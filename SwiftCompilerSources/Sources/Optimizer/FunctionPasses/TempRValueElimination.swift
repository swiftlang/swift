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

  guard copy.isInitializationOfDestination,
        let allocStack = copy.destinationAddress as? AllocStackInst
  else {
    return
  }

  if keepDebugInfo, allocStack.isFromVarDecl || allocStack.isLexical {
    return
  }

  var allocStackUses = UseCollector(copy: copy, context)
  defer { allocStackUses.deinitialize() }

  // Scan all uses of the `allocStack` to verify only the `copy` is writing to it.
  guard allocStackUses.collectUses(of: allocStack) else {
    return
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
    return
  }

  var liverange = Liverange(context)
  defer { liverange.deinitialize() }

  guard liverange.compute(for: copy, users: allocStackUses.users),
        liverange.areAllUsersInLiverange(of: allocStack),
        liverange.canExtendAccessScopes()
  else {
    return
  }

  liverange.moveDebugValuesIntoLiverange(of: allocStack, after: copy.loadingInstruction)
  liverange.extendAccessScopes()

  if !copy.isTakeOfSource {
    removeDestroys(users: allocStackUses.users, context)
  }

  allocStack.uses.ignore(usersOfType: DeallocStackInst.self).replaceAll(with: copy.sourceAddress, context)

  if keepDebugInfo {
    Builder(before: copy, context).createDebugStep()
  }
  if let debugVar = allocStack.debugVariable {
    let builder = Builder(after: copy.loadingInstruction, location: allocStack.location, context)
    builder.createDebugValue(value: copy.sourceAddress, debugVariable: debugVar)
  }
  context.erase(instructionIncludingAllUsers: copy.loadingInstruction)
  context.erase(instructionIncludingAllUsers: allocStack)
}

private func removeDestroys(users: Stack<Instruction>, _ context: FunctionPassContext) {
  for user in users {
    switch user {
    case is DestroyAddrInst:
      context.erase(instruction: user)
    case let cai as CopyAddrInst where cai.isTakeOfSource:
      cai.set(isTakeOfSource: false, context)
    case let load as LoadInst where load.loadOwnership == .take:
      load.set(ownership: .copy, context)
    default:
      // Note that no operations other than the cases above can destroy the `allocStack` (we checked
      // this in the `UseCollector`).
      break
    }
  }
}

private extension AllocStackInst {
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

/// Collects all uses of the `alloc_stack`.
private struct UseCollector : AddressDefUseWalker {
  private(set) var users: Stack<Instruction>
  private let copy: CopyLikeInstruction

  init(copy: CopyLikeInstruction, _ context: FunctionPassContext) {
    self.users = Stack(context)
    self.copy = copy
  }

  mutating func collectUses(of allocStack: AllocStackInst) -> Bool {
    if walkDownUses(ofAddress: allocStack, path: UnusedWalkingPath()) == .abortWalk {
      return false
    }
    return true
  }

  public mutating func walkDown(address operand: Operand, path: UnusedWalkingPath) -> WalkResult {
    switch operand.instruction {
    case let openExistential as OpenExistentialAddrInst:
      if !openExistential.isImmutable {
        return.abortWalk
      }
    case let takeEnum as UncheckedTakeEnumDataAddrInst:
      // In certain cases, `unchecked_take_enum_data_addr` invalidates the underlying memory.
      if takeEnum.mayBeDestructive {
        return .abortWalk
      }
    case let beginAccess as BeginAccessInst:
      if beginAccess.accessKind != .read {
        return .abortWalk
      }
      users.append(contentsOf: beginAccess.scopeEndingOperands.users)
    case let dropDeinit as DropDeinitInst:
      // `drop_deinit` is a side-effect instruction can can meaningfully exist without any users.
      // Therefore we have to explicitly add it to `users`.
      users.append(dropDeinit)
    default:
      break
    }
    return walkDownDefault(address: operand, path: path)
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    if address.isTypeDependent {
      return .continueWalk
    }

    if address.instruction == copy {
      return .continueWalk
    }

    // Only allow uses that cannot destroy their operand. We need to be sure that replacing all the uses
    // with the copy source doesn't destroy the source.
    switch address.instruction {
    case let beginApply as BeginApplyInst:
      users.append(beginApply)
      // Extend the lifetime of the `alloc_stack` to the 'end_apply'/'abort_apply'.
      users.append(contentsOf: beginApply.token.uses.users)
      return visitApply(address: address, apply: beginApply)

    case let partialApply as PartialApplyInst:
      users.append(partialApply)
      if !partialApply.isOnStack {
        return .abortWalk
      }
      return visitApply(address: address, apply: partialApply)

    case let apply as ApplySite:
      // Remaining applies: `apply` and `try_apply`
      users.append(apply)
      return visitApply(address: address, apply: apply)

    case let yield as YieldInst:
      users.append(yield)
      if !yield.convention(of: address).isGuaranteed {
        return .abortWalk
      }
      return .continueWalk

    case let addrCast as UncheckedAddrCastInst:
      return walkDownUses(ofAddress: addrCast, path: UnusedWalkingPath())

    case let loadBorrow as LoadBorrowInst:
      users.append(loadBorrow)
      for end in loadBorrow.uses.endingLifetime.users {
        if end is BranchInst {
          return .abortWalk
        }
        users.append(end)
      }
      return .continueWalk

    case let copyFromStack as CopyAddrInst:
      users.append(copyFromStack)
      if copyFromStack.destinationOperand == address {
        return .abortWalk
      }
      return .continueWalk

    case is LoadInst, is FixLifetimeInst, is DestroyAddrInst, is SwitchEnumAddrInst:
      users.append(address.instruction)
      return .continueWalk

    case is DebugValueInst, is DeallocStackInst:
      return .continueWalk

    default:
      return .abortWalk
    }
  }

  private mutating func visitApply(address: Operand, apply: ApplySite) -> WalkResult {
    let argConvention = apply.convention(of: address)!
    guard argConvention.isGuaranteed ||
          (argConvention == .indirectIn && copy.isTakeOfSource)
    else {
      return .abortWalk
    }
    return .continueWalk
  }

  public mutating func unmatchedPath(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    return .abortWalk
  }

  mutating func deinitialize() {
    users.deinitialize()
  }
}

/// Represents the liverange of the `alloc_stack` - from the `copy` instruction until its last uses:
/// ```
///   %1 = alloc_stack $T
///   copy_addr %src to %1                  -+
///   ...                                    |
///   %3 = load %1                           |  liverange
///   ...                                    |
///   %4 = load %1   // last use of %1      -+
///   ...
///   dealloc_stack %1
/// ```
/// If the copy is a `load`-`store` pair, the liverange starts at the `load`.
private struct Liverange {
  var liverange: InstructionWorklist

  // The found `end_access` instructions of the source value, within the liverange.
  var endAccesses: Stack<EndAccessInst>

  let context: FunctionPassContext

  init(_ context: FunctionPassContext) {
    liverange = InstructionWorklist(context)
    endAccesses = Stack(context)
    self.context = context
  }

  mutating func deinitialize() {
    endAccesses.deinitialize()
    liverange.deinitialize()
  }

  /// Computes the liverange and returns true if there are no writes to the copy source within the liverange.
  /// Also, collects all `end_access` instructions of the copy source in `endAccesses`.
  mutating func compute(for copy: CopyLikeInstruction, users: Stack<Instruction>) -> Bool {
    let loadFromSource = copy.loadingInstruction

    // For now, let the liverange go _until_, but not _including_ the users. If a user both reads from the
    // `alloc_stack` and writes to the source, the read happens before the write and we can accept such an
    // instruction, e.g. `copy_addr %stack to %source`.
    for user in users {
      switch user {
      case let apply as FullApplySite:
        // Function calls are an exception: in a called function a potential modification of source could
        // occur _before_ the read of the `alloc_stack` (which is passed as an indirect argument).
        liverange.pushIfNotVisited(apply)
      case let destroy as DestroyAddrInst:
        // If `copy` actually _copies_ the source, all of `alloc_stack`s destroys are removed and we don't
        // need to add them to the liverange.
        if copy.isTakeOfSource {
          liverange.pushPredecessors(of: destroy, ignoring: loadFromSource)
        }
      default:
        liverange.pushPredecessors(of: user, ignoring: loadFromSource)
      }
    }

    let aliasAnalysis = context.aliasAnalysis

    while let inst = liverange.pop() {
      if inst.mayWriteToSource(address: copy.sourceAddress, aliasAnalysis) {
        return false
      }
      if let endAccess = inst as? EndAccessInst,
         aliasAnalysis.mayAlias(copy.sourceAddress, endAccess.beginAccess.address),
         // There cannot be any aliasing modifying accesses within the liverange of the `alloc_stack`,
         // because we would have cought this with `inst.mayWrite` above.
         // However, there are cases where `aliasAnalysis.mayAlias` is less precise than `Instruction.mayWrite`.
         // Therefore, just ignore any non-read accesses.
         endAccess.beginAccess.accessKind == .read
      {
        endAccesses.append(endAccess)
      }
      liverange.pushPredecessors(of: inst, ignoring: loadFromSource)
    }

    if liverange.hasBeenPushed(copy.parentFunction.entryBlock.instructions.first!) {
      // Liverange computation should never go beyond the copy instruction, because the copy is the only
      // write to the `alloc_stack` and therefore must dominate all users.
      // If we reach the function entry instruction, something must have gone wrong.
      // To be on the safe side, let's check this and abort in this case.
      return false
    }

    // Finally push the user instructions themselves (which we excluded in the first place).
    liverange.pushIfNotVisited(contentsOf: users.lazy.filter{ !($0 is DestroyAddrInst)})
    return true
  }

  /// Returns true if all users of `alloc_stack` are in the computed liverange.
  /// This might not be the case if there are any users before the copy instruction.
  /// Ignore `debug_value` because we can move this instruction easily.
  func areAllUsersInLiverange(of allocStack: AllocStackInst) -> Bool {
    for user in allocStack.users {
      if !liverange.hasBeenPushed(user) {
        switch user {
        // Ignore instructions which are not added to `UseCollector.users`, but are not relevant,
        // because the will be deleted or can be moved.
        case is DeallocStackInst, is DestroyAddrInst, is CopyAddrInst, is DebugValueInst:
          break
        default:
          return false
        }
      }
    }
    return true
  }

  /// Move `debug_value` instructions, which are located _before_ the copy instruction, after the copy instruction.
  func moveDebugValuesIntoLiverange(of allocStack: AllocStackInst, after: Instruction) {
    for user in allocStack.users {
      if !liverange.hasBeenPushed(user) {
        switch user {
        case let debugValue as DebugValueInst:
          debugValue.move(before: after.next!, context)
        case is DeallocStackInst, is DestroyAddrInst, is CopyAddrInst:
          break
        default:
          fatalError("moving this kind of instruction is currently not supported")
        }
      }
    }
  }

  /// Check if we can move `end_access`es down to extend access scopes over all uses of the `alloc_stack`.
  /// For example:
  /// ```
  ///   %a = begin_access %src
  ///   copy_addr %a to [init] %temp : $*T
  ///   end_access %a
  ///   use %temp
  /// ```
  /// We must not replace %temp with %a after the `end_access`. Instead we try to move the `end_access`
  /// after the last use.
  func canExtendAccessScopes() -> Bool {
    let aliasAnalysis = context.aliasAnalysis
    var endAccessBlocks = BasicBlockSet(context)
    defer { endAccessBlocks.deinitialize() }

    for endAccess in endAccesses {
      guard endAccessBlocks.insert(endAccess.parentBlock) else {
        // For simplicity, only handle a single `end_access` per block (which is usually the case).
        return false
      }
      let accessAddr = endAccess.beginAccess.address
      for inst in InstructionList(first: endAccess.next) {
        if !liverange.hasBeenPushed(inst) {
          break
        }
        switch inst {
        case is BeginAccessInst, is BeginUnpairedAccessInst, is EndAccessInst,
             is TermInst:
          // We cannot move an `end_access` over a `begin_access`. This would destroy the proper nesting of accesses.
          return false
        default:
          // Don't extend a read-access scope over a (potential) write.
          // Note that `inst` can be a function call containing other access scopes. But doing the `inst.mayWrite`
          // check, we know that the function can only contain read accesses (to the same memory location).
          // So it's fine to move `endAccessToMove` even over such a function call.
          if inst.mayWriteToSource(address: accessAddr, aliasAnalysis) {
            return false
          }
        }
      }
    }
    return true
  }

  func extendAccessScopes() {
    for endAccess in endAccesses {
      for inst in InstructionList(first: endAccess.next) {
        if !liverange.hasBeenPushed(inst) {
          endAccess.move(before: inst, context)
          break
        }
        assert(!(inst is TermInst), "no place found to move end_access")
      }
    }
  }

}

private extension Instruction {
  func mayWriteToSource(address: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    switch self {
    case is FixLifetimeInst:
      // fix_lifetime has memory-write effects defined. However, in TempRValueElimination we
      // don't shrink lifetimes. Therefore we can safely ignore this instruction.
      return false
    default:
      return mayWrite(toAddress: address, aliasAnalysis)
    }
  }
}
