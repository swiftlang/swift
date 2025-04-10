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
/// * In caseof a `load`: during the (forward-extended) lifetime of the loaded value the
///                       memory location is not changed.
/// * In case of a `copy_value`: the (guaranteed) lifetime of the source operand extends
///                       the lifetime of the copied value.
/// * All (forward-extended) uses of the load or copy support guaranteed ownership.
/// * The (forward-extended) lifetime of the load or copy ends with `destroy_value`(s).
///
let copyToBorrowOptimization = FunctionPass(name: "copy-to-borrow-optimization") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership {
    return
  }

  for inst in function.instructions {
    switch inst {
    case let load as LoadInst:
      optimize(load: load, context)
    case let copy as CopyValueInst:
      optimize(copy: copy, context)
    default:
      break
    }
  }
}

private func optimize(load: LoadInst, _ context: FunctionPassContext) {
  if load.loadOwnership != .copy {
    return
  }

  var collectedUses = Uses(context)
  defer { collectedUses.deinitialize() }
  if !collectedUses.collectUses(of: load) {
    return
  }

  if mayWrite(toAddressOf: load,
              within: collectedUses.destroys,
              usersInDeadEndBlocks: collectedUses.usersInDeadEndBlocks,
              context)
  {
    return
  }

  load.replaceWithLoadBorrow(collectedUses: collectedUses)
}

private func optimize(copy: CopyValueInst, _ context: FunctionPassContext) {
  if copy.fromValue.ownership != .guaranteed {
    return
  }

  var collectedUses = Uses(context)
  defer { collectedUses.deinitialize() }
  if !collectedUses.collectUses(of: copy) {
    return
  }

  var liverange = InstructionRange(begin: copy, context)
  defer { liverange.deinitialize() }
  liverange.insert(contentsOf: collectedUses.destroys)
  liverange.insert(contentsOf: collectedUses.usersInDeadEndBlocks)

  if !liverange.isFullyContainedIn(borrowScopeOf: copy.fromValue.lookThroughForwardingInstructions) {
    return
  }

  remove(copy: copy, collectedUses: collectedUses, liverange: liverange)
}

private struct Uses {
  let context: FunctionPassContext

  // Operand of all forwarding instructions, which - if possible - are converted from "owned" to "guaranteed"
  private(set) var forwardingUses: Stack<Operand>

  // All destroys of the load/copy_value and its forwarded values.
  private(set) var destroys: Stack<DestroyValueInst>

  // Exit blocks of the load/copy_value's liverange which don't have a destroy.
  // Those are successor blocks of terminators, like `switch_enum`, which do _not_ forward the value.
  // E.g. the none-case of a switch_enum of an Optional.
  private(set) var nonDestroyingLiverangeExits: Stack<Instruction>

  private(set) var usersInDeadEndBlocks: Stack<Instruction>

  init(_ context: FunctionPassContext) {
    self.context = context
    self.forwardingUses = Stack(context)
    self.destroys = Stack(context)
    self.nonDestroyingLiverangeExits = Stack(context)
    self.usersInDeadEndBlocks = Stack(context)
  }

  mutating func collectUses(of initialValue: SingleValueInstruction) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    // If the load/copy_value is immediately followed by a single `move_value`, use the moved value.
    // Note that `move_value` is _not_ a forwarding instruction.
    worklist.pushIfNotVisited(initialValue.singleMoveValueUser ?? initialValue)

    while let value = worklist.pop() {
      for use in value.uses.endingLifetime {
        switch use.instruction {
        case let destroy as DestroyValueInst:
          destroys.append(destroy)

        case let forwardingInst as ForwardingInstruction where forwardingInst.canChangeToGuaranteedOwnership:
          forwardingUses.append(use)
          findNonDestroyingLiverangeExits(of: forwardingInst)
          worklist.pushIfNotVisited(contentsOf: forwardingInst.forwardedResults.lazy.filter { $0.ownership == .owned})
        default:
          return false
        }
      }
      // Get potential additional uses in dead-end blocks for which a final destroy is missing.
      // In such a case the dataflow would _not_ visit potential writes to the load's memory location.
      // In the following example, the `load [copy]` must not be converted to a `load_borrow`:
      //
      //   %1 = load [copy] %0
      //     ...
      //   store %2 to %0
      //     ...
      //   use of %1      // additional use: the lifetime of %1 ends here
      //     ...          // no destroy of %1!
      //   unreachable
      //
      // TODO: we can remove this once with have completed OSSA lifetimes throughout the SIL pipeline.
      findAdditionalUsesInDeadEndBlocks(of: value)
    }
    return true
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

  private mutating func findAdditionalUsesInDeadEndBlocks(of value: Value) {
    var users = Stack<Instruction>(context)
    defer { users.deinitialize() }

    // Finds all uses except destroy_value.
    var visitor = InteriorUseWalker(definingValue: value, ignoreEscape: true, visitInnerUses: true, context) {
      let user = $0.instruction
      if !(user is DestroyValueInst) {
        users.append(user)
      }
      return .continueWalk
    }
    defer { visitor.deinitialize() }

    _ = visitor.visitUses()
    usersInDeadEndBlocks.append(contentsOf: users)
  }

  mutating func deinitialize() {
    forwardingUses.deinitialize()
    destroys.deinitialize()
    nonDestroyingLiverangeExits.deinitialize()
    usersInDeadEndBlocks.deinitialize()
  }
}

private func mayWrite(
  toAddressOf load: LoadInst,
  within destroys: Stack<DestroyValueInst>,
  usersInDeadEndBlocks: Stack<Instruction>,
  _ context: FunctionPassContext
) -> Bool {
  let aliasAnalysis = context.aliasAnalysis
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  for destroy in destroys {
    worklist.pushPredecessors(of: destroy, ignoring: load)
  }
  worklist.pushIfNotVisited(contentsOf: usersInDeadEndBlocks)

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

    replaceMoveWithBorrow(of: self, replacedBy: loadBorrow, liverange: liverange, collectedUses: collectedUses)
    createEndBorrows(for: loadBorrow, atEndOf: liverange, collectedUses: collectedUses)

    uses.replaceAll(with: loadBorrow, context)
    context.erase(instruction: self)

    for forwardingUse in collectedUses.forwardingUses {
      forwardingUse.changeOwnership(from: .owned, to: .guaranteed, context)
    }
    context.erase(instructions: collectedUses.destroys)
  }
}

private func remove(copy: CopyValueInst, collectedUses: Uses, liverange: InstructionRange) {
  let context = collectedUses.context
  replaceMoveWithBorrow(of: copy, replacedBy: copy.fromValue, liverange: liverange, collectedUses: collectedUses)
  copy.replace(with: copy.fromValue, context)

  for forwardingUse in collectedUses.forwardingUses {
    forwardingUse.changeOwnership(from: .owned, to: .guaranteed, context)
  }
  context.erase(instructions: collectedUses.destroys)
}

// Handle the special case if the `load` or `copy_value` is immediately followed by a single `move_value`.
// In this case we have to preserve the move's flags by inserting a `begin_borrow` with the same flags.
// For example:
//
//   %1 = load [copy] %0
//   %2 = move_value [lexical] %1
//    ...
//   destroy_value %2
// ->
//   %1 = load_borrow %0
//   %2 = begin_borrow [lexical] %1
//     ...
//   end_borrow %2
//   end_borrow %1
//
private func replaceMoveWithBorrow(
  of value: Value,
  replacedBy newValue: Value,
  liverange: InstructionRange,
  collectedUses: Uses
) {
  guard let moveInst = value.singleMoveValueUser else {
    return
  }
  let context = collectedUses.context

  // An inner borrow is needed to keep the flags of the `move_value`.
  let builder = Builder(before: moveInst, context)
  let bbi = builder.createBeginBorrow(of: newValue,
                                      isLexical: moveInst.isLexical,
                                      hasPointerEscape: moveInst.hasPointerEscape,
                                      isFromVarDecl: moveInst.isFromVarDecl)
  moveInst.replace(with: bbi, context)
  createEndBorrows(for: bbi, atEndOf: liverange, collectedUses: collectedUses)
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

private extension InstructionRange {
  func isFullyContainedIn(borrowScopeOf value: Value) -> Bool {
    guard let beginBorrow = BeginBorrowValue(value.lookThroughForwardingInstructions) else {
      return false
    }
    if case .functionArgument = beginBorrow {
      // The lifetime of a guaranteed function argument spans over the whole function.
      return true
    }
    for endOp in beginBorrow.scopeEndingOperands {
      if self.contains(endOp.instruction) {
        return false
      }
    }
    return true
  }
}

private extension Value {
  var singleMoveValueUser: MoveValueInst? {
    uses.ignoreDebugUses.singleUse?.instruction as? MoveValueInst
  }

  var lookThroughForwardingInstructions: Value {
    if let bfi = definingInstruction as? BorrowedFromInst,
       !bfi.borrowedPhi.isReborrow,
       bfi.enclosingValues.count == 1
    {
      // Return the single forwarded enclosingValue
      return bfi.enclosingValues[0]
    }
    if let fi = definingInstruction as? ForwardingInstruction,
       let forwardedOp = fi.singleForwardedOperand
    {
       return forwardedOp.value.lookThroughForwardingInstructions
    } else if let termResult = TerminatorResult(self),
              let fi = termResult.terminator as? ForwardingInstruction,
              let forwardedOp = fi.singleForwardedOperand
    {
      return forwardedOp.value.lookThroughForwardingInstructions
    }
    return self
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
