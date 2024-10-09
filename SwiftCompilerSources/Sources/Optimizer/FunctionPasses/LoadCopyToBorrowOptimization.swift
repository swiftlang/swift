//===--- LoadCopyToBorrowOptimization.swift --------------------------------==//
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

/// Replaces a `load [copy]` with a `load_borrow` if possible.
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
/// The optimization can be done if:
/// * During the (forward-extended) lifetime of the loaded value the memory location is not changed.
/// * All (forward-extended) uses of the loaded value support guaranteed ownership.
/// * The (forward-extended) lifetime of the loaded value ends with `destroy_value`(s).
///
let loadCopyToBorrowOptimization = FunctionPass(name: "load-copy-to-borrow-optimization") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership {
    return
  }

  for inst in function.instructions {
    if let load = inst as? LoadInst {
      optimize(load: load, context)
    }
  }
}

private func optimize(load: LoadInst, _ context: FunctionPassContext) {
  if load.loadOwnership != .copy {
    return
  }

  var ownershipUses = OwnershipUses(context)
  defer { ownershipUses.deinitialize() }

  // Handle the special case if the `load` is immediately followed by a single `move_value`
  // (see the comment in `LoadInst.replaceWithLoadBorrow`).
  let loadedValue = load.singleMoveValueUser ?? load

  if !ownershipUses.collectUses(of: loadedValue) {
    return
  }

  var liverange = InstructionWorklist(liverangeOf: load, with: ownershipUses)
  defer { liverange.deinitialize() }

  if liverange.mayWrite(toAddressOf: load, context) {
    return
  }

  load.replaceWithLoadBorrow(liverange: liverange, ownershipUses: ownershipUses)
}

private extension LoadInst {
  var singleMoveValueUser: MoveValueInst? {
    uses.ignoreDebugUses.singleUse?.instruction as? MoveValueInst
  }

  func replaceWithLoadBorrow(liverange: InstructionWorklist, ownershipUses: OwnershipUses) {
    let context = ownershipUses.context
    let builder = Builder(before: self, context)
    let loadBorrow = builder.createLoadBorrow(fromAddress: address)

    // Handle the special case if the `load` is immediately followed by a single `move_value`.
    // Note that `move_value` is _not_ a forwarding instruction.
    // In this case we have to preserve the move's flags by inserting a `begin_borrow` with the same flags.
    // For example:
    //
    //   %1 = load [copy] %0
    //   %2 = move_value [lexical] %1
    //    ...
    //   destroy_value %2
    // ->
    //    %1 = load_borrow %0
    //    %2 = begin_borrow [lexical] %1
    //     ...
    //    end_borrow %2
    //    end_borrow %1
    //
    let innerBorrow: BeginBorrowInst?
    if let moveInst = singleMoveValueUser {
      // An inner borrow is needed to keep the flags of the `move_value`.
      let bbi = builder.createBeginBorrow(of: loadBorrow,
                                          isLexical: moveInst.isLexical,
                                          hasPointerEscape: moveInst.hasPointerEscape,
                                          isFromVarDecl: moveInst.isFromVarDecl)
      moveInst.uses.replaceAll(with: bbi, context)
      context.erase(instruction: moveInst)
      innerBorrow = bbi
    } else {
      innerBorrow = nil
    }

    uses.replaceAll(with: loadBorrow, context)
    context.erase(instruction: self)

    for destroy in ownershipUses.destroys {
      context.createEndBorrows(ofInner: innerBorrow, ofOuter: loadBorrow, before: destroy, ifNotIn: liverange)
      context.erase(instruction: destroy)
    }

    for forwardingUse in ownershipUses.forwardingUses {
      forwardingUse.changeOwnership(from: .owned, to: .guaranteed, context)
    }

    for liverangeExitBlock in ownershipUses.nonDestroyingLiverangeExits {
      context.createEndBorrows(ofInner: innerBorrow, ofOuter: loadBorrow,
                               before: liverangeExitBlock.instructions.first!,
                               ifNotIn: liverange)
    }
  }
}

private extension FunctionPassContext {
  func createEndBorrows(
    ofInner innerBorrow: BeginBorrowInst?,
    ofOuter outerBorrow: LoadBorrowInst,
    before insertionPoint: Instruction,
    ifNotIn liverange: InstructionWorklist
  ) {
    // There can be multiple destroys in a row in case of decomposing an aggregate, e.g.
    //   %1 = load [copy] %0
    //     ...
    //   (%2, %3) = destructure_struct %1
    //   destroy_value %2
    //   destroy_value %3  // The final destroy. Here we need to create the `end_borrow`(s)
    //
    if liverange.hasBeenPushed(insertionPoint) {
      return
    }
    let builder = Builder(before: insertionPoint, self)
    if let innerBorrow = innerBorrow {
      builder.createEndBorrow(of: innerBorrow)
    }
    builder.createEndBorrow(of: outerBorrow)
  }

}

private struct OwnershipUses {
  let context: FunctionPassContext

  // Operand of all forwarding instructions, which - if possible - are converted from "owned" to "guaranteed"
  private(set) var forwardingUses: Stack<Operand>

  // All destroys of the load its forwarded values.
  private(set) var destroys: Stack<DestroyValueInst>

  // Uses of the load in a dead-end block where the final `destroy_value` is missing at the `unreachable`.
  // In such a case the dataflow would _not_ visit potential writes to the load's memory location.
  // In the following example, the `load [copy]` must not be converted to a `load_borrow`:
  //
  //   %1 = load [copy] %0
  //     ...
  //   store %2 to %0
  //     ...
  //   use of %1      // additional last use: the lifetime of %1 ends here
  //     ...          // no destroy of %1!
  //   unreachable
  //
  // TODO: we can remove this once with have completed OSSA lifetimes throughout the SIL pipeline.
  private(set) var additionalLastUses: Stack<Instruction>

  // Exit blocks of the load's liverange which don't have a destroy.
  // Those are successor blocks of terminators, like `switch_enum`, which do _not_ forward the value.
  // E.g. the none-case of a switch_enum of an Optional.
  private(set) var nonDestroyingLiverangeExits: Stack<BasicBlock>

  init(_ context: FunctionPassContext) {
    self.context = context
    self.forwardingUses = Stack(context)
    self.destroys = Stack(context)
    self.additionalLastUses = Stack(context)
    self.nonDestroyingLiverangeExits = Stack(context)
  }

  mutating func collectUses(of ownedValue: Value) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(ownedValue)

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
      // Get potential additional last uses in dead-end blocks for which the final destroy is missing.
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
        nonDestroyingLiverangeExits.append(succ)
      }
    }
  }

  private mutating func findAdditionalUsesInDeadEndBlocks(of value: Value) {
    var ln = computeKnownLiveness(for: value, visitInnerUses: true, context)
    defer { ln.deinitialize() }
    additionalLastUses.append(contentsOf: ln.ends.filter { !($0 is DestroyValueInst)})
  }

  mutating func deinitialize() {
    forwardingUses.deinitialize()
    destroys.deinitialize()
    additionalLastUses.deinitialize()
    nonDestroyingLiverangeExits.deinitialize()
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


private extension InstructionWorklist {

  init(liverangeOf load: LoadInst, with ownershipUses: OwnershipUses) {
    self.init(ownershipUses.context)

    for destroy in ownershipUses.destroys {
      pushPredecessors(of: destroy, ignoring: load)
    }
    pushIfNotVisited(contentsOf: ownershipUses.additionalLastUses)
  }

  mutating func mayWrite(toAddressOf load: LoadInst, _ context: FunctionPassContext) -> Bool {
    let aliasAnalysis = context.aliasAnalysis

    while let inst = pop() {
      if inst.mayWrite(toAddress: load.address, aliasAnalysis) {
        return true
      }
      pushPredecessors(of: inst, ignoring: load)
    }
    return false
  }
}
