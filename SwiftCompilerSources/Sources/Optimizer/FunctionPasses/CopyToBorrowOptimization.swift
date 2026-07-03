//===--- CopyToBorrowOptimization.swift ------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// 1. replaces a `load [copy]` with a `load_borrow` if possible:
///
/// ```
///   %1 = load [copy] %0
///   // no writes to %0
///   destroy_value %1
/// ```
/// ->
/// ```
///   %1 = load_borrow %0
///   // no writes to %0
///   end_borrow %1
/// ```
///
/// 2. removes a `copy_value` where the source is a guaranteed value, if possible:
///
/// ```
///   %1 = copy_value %0   // %0 = a guaranteed value
///   // uses of %1
///   destroy_value %1     // borrow scope of %0 is still valid here
/// ```
/// ->
/// ```
///   // uses of %0
/// ```

/// The optimization can be done if:
/// * In case of a `load`: during the (forward-extended) lifetime of the loaded value the
///                       memory location is not changed.
/// * In case of a `copy_value`: the lifetime of the source operand extends - or can be extended to -
///                       the lifetime of the copied value.
/// * All (forward-extended) uses of the load or copy support guaranteed ownership. This includes stores
///     to stack locations which can be converted to `store_borrow`.
/// * The (forward-extended) lifetime of the load or copy ends with `destroy_value`(s).
///
/// As an additional related optimization, "dead" `copy_value` instructions are removed:
/// ```
///   %2 = copy_value %1
///   ...                // no deinit barriers here
///   destroy_value %2   // the only use of %2
/// ```
///
let copyToBorrowOptimization = FunctionPass(name: "copy-to-borrow-optimization") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership {
    return
  }

  var changed = false

  for inst in function.instructions {
    switch inst {
    case let load as LoadInst:
      if !context.continueWithNextSubpassRun(for: load) {
        return
      }
      if optimize(load: load, context) {
        changed = true
      }
    case let copy as CopyValueInst:
      if !context.continueWithNextSubpassRun(for: copy) {
        return
      }
      if optimize(copy: copy, context) {
        changed = true
        break
      }
      if removeDead(copy: copy, context) {
        changed = true
      }
    default:
      break
    }
  }

  if changed {
    updateBorrowedFrom(in: function, context)
  }
}

private func optimize(load: LoadInst, _ context: FunctionPassContext) -> Bool {
  if load.loadOwnership != .copy {
    return false
  }

  var collectedUses = Uses(context)
  defer { collectedUses.deinitialize() }
  if !collectedUses.collectUses(of: load) {
    return false
  }

  if mayWrite(toAddressOf: load, within: collectedUses.destroys, context) {
    return false
  }

  load.replaceWithLoadBorrow(collectedUses: collectedUses)
  return true
}

private func optimize(copy: CopyValueInst, _ context: FunctionPassContext) -> Bool {
  var collectedUses = Uses(context)
  defer { collectedUses.deinitialize() }
  if !collectedUses.collectUses(of: copy) {
    return false
  }

  var liverange = InstructionRange(begin: copy, context)
  defer { liverange.deinitialize() }
  liverange.insert(contentsOf: collectedUses.destroys)

  if copy.fromValue.ownership == .owned {
    if !liverange.isFullyContainedIn(scopeOf: copy.fromValue) {
      return false
    }
  } else {
    guard extendBorrowScope(of: copy.fromValue, toOverlap: liverange, context) else {
      return false
    }
  }

  remove(copy: copy, collectedUses: collectedUses, liverange: liverange)
  return true
}

/// Removes a `copy_value` if the result is only destroyed and there are not deinit-barriers
/// between the copy and the `destroy_value`s.
private func removeDead(copy: CopyValueInst, _ context: FunctionPassContext) -> Bool {
  guard copy.uses.ignoreDebugUses.users.allSatisfy({ $0 is DestroyValueInst }) else {
    return false
  }
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  for user in copy.users {
    worklist.pushPredecessors(of: user)
  }
  let calleeAnalysis = context.calleeAnalysis
  while let inst = worklist.pop() {
    if inst.isDeinitBarrier(calleeAnalysis) {
      return false
    }
    worklist.pushPredecessors(of: inst, ignoring: copy)
  }
  context.erase(instructionIncludingAllUsers: copy)
  return true
}

private struct Uses {
  let context: FunctionPassContext

  // Operand of all forwarding instructions, which - if possible - are converted from "owned" to "guaranteed"
  private(set) var forwardingUses: Stack<Operand>

  // All destroys of the load/copy_value and its forwarded values.
  private(set) var destroys: Stack<Instruction>

  // Exit blocks of the load/copy_value's liverange which don't have a destroy.
  // Those are successor blocks of terminators, like `switch_enum`, which do _not_ forward the value.
  // E.g. the none-case of a switch_enum of an Optional.
  private(set) var nonDestroyingLiverangeExits: Stack<Instruction>

  init(_ context: FunctionPassContext) {
    self.context = context
    self.forwardingUses = Stack(context)
    self.destroys = Stack(context)
    self.nonDestroyingLiverangeExits = Stack(context)
  }

  mutating func collectUses(of initialValue: SingleValueInstruction) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    // If the load/copy_value is immediately followed by a single `move_value`, use the moved value.
    // Note that `move_value` is _not_ a forwarding instruction.
    worklist.pushIfNotVisited(initialValue)

    while let value = worklist.pop() {
      for use in value.uses.endingLifetime {
        switch use.instruction {
        case let destroy as DestroyValueInst:
          destroys.append(destroy)

        case let forwardingInst as ForwardingInstruction where forwardingInst.canChangeToGuaranteedOwnership:
          forwardingUses.append(use)
          findNonDestroyingLiverangeExits(of: forwardingInst)
          worklist.pushIfNotVisited(contentsOf: forwardingInst.forwardedResults.lazy.filter { $0.ownership == .owned})

        case let store as StoreInst:
          assert(use == store.sourceOperand)
          guard canConvertToStoreBorrow(store: store, destroys: &destroys, context) else {
            return false
          }
          forwardingUses.append(use)
        default:
          return false
        }
      }
    }
    return true
  }

  func changeOwnedToGuaranteed(outerScope: Value) {
    for forwardingUse in forwardingUses {
      switch forwardingUse.instruction {
      case let store as StoreInst:
        changeStoreToStoreBorrow(store: store, outerScope: outerScope)
      default:
        forwardingUse.changeOwnership(from: .owned, to: .guaranteed, context)
      }
    }
    context.erase(instructions: destroys)
  }

  private func changeStoreToStoreBorrow(store: StoreInst, outerScope: Value) {
    let allocStack = store.destination
    let builder = Builder(before: store, context)
    let storeBorrow = builder.createStoreBorrow(source: store.source, destination: allocStack)

    for use in allocStack.uses {
      switch use.instruction {
      case storeBorrow, is DeallocStackInst:
        break
      case let destroy as DestroyAddrInst:
        if let prev = destroy.previous,
           let endBorrow = prev as? EndBorrowInst,
           endBorrow.borrow == outerScope
        {
          // If we already inserted new `end_borrow`s for an outer scope we need to make sure that the
          // `end_borrow`s for the `store_borrow` (= an inner scope) are inserted before the `end_borrow`s
          // of the outer scope.
          Builder(before: endBorrow, context).createEndBorrow(of: storeBorrow)
        } else {
          Builder(before: destroy, context).createEndBorrow(of: storeBorrow)
        }
      default:
        use.set(to: storeBorrow, context)
      }
    }
    context.erase(instruction: store)

  }

  private mutating func findNonDestroyingLiverangeExits(of forwardingInst: ForwardingInstruction) {
    if let termInst = forwardingInst as? TermInst {
      // A terminator instruction can implicitly end the lifetime of its operand in a success block,
      // e.g. a `switch_enum` with a non-payload case block. Such success blocks need an `end_borrow`, though.
      for succ in termInst.successors where !succ.arguments.contains(where: {$0.ownership == .owned}) {
        nonDestroyingLiverangeExits.append(succ.instructions.first!)
      }
    } else if !forwardingInst.forwardedResults.contains(where: { $0.ownership == .owned }) {
      // The forwarding instruction has no owned result, which means it ends the lifetime of its owned operand.
      // This can happen with an `unchecked_enum_data` which extracts a trivial payload out of a
      // non-trivial enum.
      nonDestroyingLiverangeExits.append(forwardingInst.next!)
    }
  }

  mutating func deinitialize() {
    forwardingUses.deinitialize()
    destroys.deinitialize()
    nonDestroyingLiverangeExits.deinitialize()
  }
}

/// Checks if the `store` stores to an `alloc_stack` and that no other instructions (beside `destroy_addr`)
/// modify the stack location.
private func canConvertToStoreBorrow(store: StoreInst,
                                     destroys: inout Stack<Instruction>,
                                     _ context: FunctionPassContext) -> Bool
{
  guard store.storeOwnership == .initialize,
        let allocStack = store.destination as? AllocStackInst
  else {
    return false
  }

  var walker = AllocStackUsesWalker(initialStore: store, context)
  defer { walker.deinitialize() }
  if walker.walkDownUses(ofAddress: allocStack, path: UnusedWalkingPath()) == .abortWalk {
    return false
  }

  guard isDestroyedOnAllPaths(allocStack: allocStack, destroys: walker.destroys, context) else {
    return false
  }

  destroys.append(contentsOf: walker.destroys)
  return true
}

private func isDestroyedOnAllPaths(allocStack: AllocStackInst,
                                   destroys: Stack<Instruction>,
                                   _ context: FunctionPassContext) -> Bool
{
  if destroys.isEmpty {
    return false
  }
  var liverange = BasicBlockRange(begin: allocStack.parentBlock, context)
  defer { liverange.deinitialize() }
  liverange.insert(contentsOf: destroys.lazy.map(\.parentBlock))
  return liverange.exits.isEmpty
}

private struct AllocStackUsesWalker : AddressDefUseWalker {
  let context: FunctionPassContext
  let initialStore: StoreInst
  var destroys: Stack<Instruction>

  init(initialStore: StoreInst, _ context: FunctionPassContext) {
    self.initialStore = initialStore
    self.context = context
    self.destroys = Stack(context)
  }

  mutating func deinitialize() {
    self.destroys.deinitialize()
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    switch address.instruction {
    case let load as LoadInst:
      if load.loadOwnership == .take {
        return .abortWalk
      }
      return .continueWalk
    case let store as StoreInst:
      if store != initialStore {
        return .abortWalk
      }
      return .continueWalk
    case let copy as SourceDestAddrInstruction:
      if address == copy.destinationOperand {
        return .abortWalk
      }
      if address == copy.sourceOperand && copy.isTakeOfSource {
        return .abortWalk
      }
      return .continueWalk
    case let apply as ApplySite:
      switch apply.convention(of: address) {
      case .indirectInGuaranteed:
        if let pa = apply as? PartialApplyInst, !pa.isOnStack {
          return .abortWalk
        }
        return .continueWalk
      default:
        return .abortWalk
      }
    case let destroy as DestroyAddrInst:
      if destroy.destroyedAddress == initialStore.destination {
        destroys.append(destroy)
        return .continueWalk
      }
      return .abortWalk
    case is DeallocStackInst, is DebugValueInst:
      return .continueWalk
    default:
      return .abortWalk
    }
  }
}

private func mayWrite(
  toAddressOf load: LoadInst,
  within destroys: Stack<Instruction>,
  _ context: FunctionPassContext
) -> Bool {
  let aliasAnalysis = context.aliasAnalysis
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  for destroy in destroys {
    worklist.pushPredecessors(of: destroy, ignoring: load)
  }

  // Visit all instructions starting from the destroys in backward order.
  while let inst = worklist.pop() {
    if inst.mayWrite(toAddress: load.address, aliasAnalysis) {
      return true
    }
    worklist.pushPredecessors(of: inst, ignoring: load)
  }
  return false
}

private extension LoadInst {
  func replaceWithLoadBorrow(collectedUses: Uses) {
    let context = collectedUses.context
    let builder = Builder(before: self, context)
    let loadBorrow = builder.createLoadBorrow(fromAddress: address)

    var liverange = InstructionRange(begin: self, ends: collectedUses.destroys, context)
    defer { liverange.deinitialize() }

    createEndBorrows(for: loadBorrow, atEndOf: liverange, collectedUses: collectedUses)

    uses.replaceAll(with: loadBorrow, context)
    context.erase(instruction: self)

    collectedUses.changeOwnedToGuaranteed(outerScope: loadBorrow)
  }
}

private func remove(copy: CopyValueInst, collectedUses: Uses, liverange: InstructionRange) {
  let context = collectedUses.context
  let fromValue = copy.fromValue

  switch fromValue.ownership {
  case .owned:
    let builder = Builder(before: copy, context)
    let beginBorrow = builder.createBeginBorrow(of: fromValue)
    copy.replace(with: beginBorrow, context)
    createEndBorrows(for: beginBorrow, atEndOf: liverange, collectedUses: collectedUses)
    collectedUses.changeOwnedToGuaranteed(outerScope: beginBorrow)
  case .guaranteed:
    copy.replace(with: fromValue, context)
    collectedUses.changeOwnedToGuaranteed(outerScope: fromValue.lookThroughForwardingInstructions)
  case .none, .unowned:
    fatalError("unexpected ownership of copy source")
  }
}

private func createEndBorrows(for beginBorrow: Value, atEndOf liverange: InstructionRange, collectedUses: Uses) {
  let context = collectedUses.context

  // There can be multiple destroys in a row in case of decomposing an aggregate, e.g.
  //   %1 = load [copy] %0
  //     ...
  //   (%2, %3) = destructure_struct %1
  //   destroy_value %2
  //   destroy_value %3  // The final destroy. Here we need to create the `end_borrow`(s)
  //

  var allLifetimeEndingInstructions = InstructionWorklist(context)
  allLifetimeEndingInstructions.pushIfNotVisited(contentsOf: collectedUses.destroys.lazy.map { $0 })
  allLifetimeEndingInstructions.pushIfNotVisited(contentsOf: collectedUses.nonDestroyingLiverangeExits)

  defer {
    allLifetimeEndingInstructions.deinitialize()
  }

  while let endInst = allLifetimeEndingInstructions.pop() {
    if !liverange.contains(endInst) {
      let builder = Builder(before: endInst, context)
      builder.createEndBorrow(of: beginBorrow)
    }
  }
}

private extension ForwardingInstruction {
  var canChangeToGuaranteedOwnership: Bool {
    if !preservesReferenceCounts {
      return false
    }
    if !canForwardGuaranteedValues {
      return false
    }
    // For simplicity only support a single owned operand. Otherwise we would have to check if the other
    // owned operands stem from `load_borrow`s, too, which we can convert, etc.
    let numOwnedOperands = operands.lazy.filter({ $0.value.ownership == .owned }).count
    if numOwnedOperands > 1 {
      return false
    }
    return true
  }
}
