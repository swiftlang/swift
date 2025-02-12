//===--- PhiUpdater.swift -------------------------------------------------===//
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
import OptimizerBridging

/// Updates the reborrow flags and the borrowed-from instructions for all guaranteed phis in `function`.
func updateGuaranteedPhis(in function: Function, _ context: some MutatingContext) {
  updateReborrowFlags(in: function, context)
  updateBorrowedFrom(in: function, context)
}

/// Updates the reborrow flags and the borrowed-from instructions for all `phis`.
func updateGuaranteedPhis(phis: some Sequence<Phi>, _ context: some MutatingContext) {
  updateReborrowFlags(for: phis, context)
  updateBorrowedFrom(for: phis, context)
}

/// Update all borrowed-from instructions in the `function`
func updateBorrowedFrom(in function: Function, _ context: some MutatingContext) {
  if !function.hasOwnership {
    return
  }
  var guaranteedPhis = Stack<Phi>(context)
  defer { guaranteedPhis.deinitialize() }

  for block in function.blocks {
    for arg in block.arguments {
      if let phi = Phi(arg), phi.value.ownership == .guaranteed {
        guaranteedPhis.append(phi)
      }
    }
  }
  updateBorrowedFrom(for: guaranteedPhis, context)
}

/// Update borrowed-from instructions for a set of phi arguments.
func updateBorrowedFrom(for phis: some Sequence<Phi>, _ context: some MutatingContext) {
  for phi in phis {
    if !phi.value.parentFunction.hasOwnership {
      return
    }
    if phi.value.ownership == .guaranteed {
      createEmptyBorrowedFrom(for: phi, context)
    }
  }

  var changed: Bool
  repeat {
    changed = false

    for phi in phis {
      if phi.value.ownership == .guaranteed {
        changed = updateBorrowedFrom(for: phi, context) || changed
      }
    }
  } while changed
}

/// Updates the reborrow flags for all guaranteed phis in `function`.
func updateReborrowFlags(in function: Function, _ context: some MutatingContext) {
  if !function.hasOwnership {
    return
  }
  var guaranteedPhis = Stack<Phi>(context)
  defer { guaranteedPhis.deinitialize() }

  for block in function.blocks.reversed() {
    for arg in block.arguments {
      if let phi = Phi(arg), phi.value.ownership == .guaranteed {
        guaranteedPhis.append(phi)
      }
    }
  }
  updateReborrowFlags(for: guaranteedPhis, context)
}

/// Updates the reborrow flags for all `phis`.
///
/// Re-borrow flags are only set, but never cleared. If an optimization creates a dead-end block
/// by cutting off the control flow before an `end_borrow`, the re-borrow flags still have to remain
/// without the possibility to re-calculate them from the (now missing) `end_borrow`.
///
func updateReborrowFlags(for phis: some Sequence<Phi>, _ context: some MutatingContext) {
  if let phi = phis.first(where: { phi in true }), !phi.value.parentFunction.hasOwnership {
    return
  }

  var changed: Bool
  repeat {
    changed = false

    for phi in phis where phi.value.ownership == .guaranteed {
      if !phi.value.isReborrow && phi.hasBorrowEndingUse {
        phi.value.set(reborrow: true, context)
        changed = true
      }
    }
  } while changed
}

private func updateBorrowedFrom(for phi: Phi, _ context: some MutatingContext) -> Bool {
  var computedEVs = Stack<Value>(context)
  defer { computedEVs.deinitialize() }
  gatherEnclosingValuesFromPredecessors(for: phi, in: &computedEVs, context)

  let borrowedFrom = phi.borrowedFrom!
  var existingEVs = ValueSet(insertContentsOf: borrowedFrom.enclosingValues, context)
  defer { existingEVs.deinitialize() }

  if computedEVs.allSatisfy({ existingEVs.contains($0) }) {
    return false
  }
  var evs = Array<Value>(borrowedFrom.enclosingValues)
  evs.append(contentsOf: computedEVs.lazy.filter { !existingEVs.contains($0) })

  let builder = Builder(before: borrowedFrom, context)
  let newBfi = builder.createBorrowedFrom(borrowedValue: borrowedFrom.borrowedValue, enclosingValues: evs)
  borrowedFrom.replace(with: newBfi, context)
  return true
}

private func createEmptyBorrowedFrom(for phi: Phi, _ context: some MutatingContext) {
  if let existingBfi = phi.borrowedFrom {
    if existingBfi.enclosingValues.isEmpty {
      return
    }
    existingBfi.replace(with: phi.value, context)
  }
  let builder = Builder(atBeginOf: phi.value.parentBlock, context)
  let bfi = builder.createBorrowedFrom(borrowedValue: phi.value, enclosingValues: [])
  phi.value.uses.ignoreUsers(ofType: BorrowedFromInst.self).replaceAll(with: bfi, context)
}

/// Replaces a phi with the unique incoming value if all incoming values are the same:
/// ```
///   bb1:
///     br bb3(%1)
///   bb2:
///     br bb3(%1)
///   bb3(%2 : $T):   // Predecessors: bb1, bb2
///     use(%2)
/// ```
/// ->
/// ```
///   bb1:
///     br bb3
///   bb2:
///     br bb3
///   bb3:
///     use(%1)
/// ```
///
func replacePhiWithIncomingValue(phi: Phi, _ context: some MutatingContext) -> Bool {
  if phi.predecessors.isEmpty {
    return false
  }
  let uniqueIncomingValue = phi.incomingValues.first!
  if !uniqueIncomingValue.parentFunction.hasOwnership {
    // For the SSAUpdater it's only required to simplify phis in OSSA.
    // This avoids that we need to handle cond_br instructions below.
    return false
  }
  if phi.incomingValues.contains(where: { $0 != uniqueIncomingValue }) {
    return false
  }
  if let borrowedFrom = phi.borrowedFrom {
    borrowedFrom.replace(with: uniqueIncomingValue, context)
  } else {
    phi.value.uses.replaceAll(with: uniqueIncomingValue, context)
  }

  let block = phi.value.parentBlock
  for incomingOp in phi.incomingOperands {
    let existingBranch = incomingOp.instruction as! BranchInst
    let argsWithRemovedPhiOp = existingBranch.operands.filter{ $0 != incomingOp }.map{ $0.value }
    Builder(before: existingBranch, context).createBranch(to: block, arguments: argsWithRemovedPhiOp)
    context.erase(instruction: existingBranch)
  }
  block.eraseArgument(at: phi.value.index, context)
  return true
}

/// Replaces phis with the unique incoming values if all incoming values are the same.
/// This is needed after running the SSAUpdater for an existing OSSA value, because the updater can
/// insert unnecessary phis in the middle of the original liverange which breaks up the original
/// liverange into smaller ones:
/// ```
///    %1 = def_of_owned_value
///    %2 = begin_borrow %1
///    ...
///    br bb2(%1)
///  bb2(%3 : @owned $T): // inserted by SSAUpdater
///    ...
///    end_borrow %2      // use after end-of-lifetime!
///    destroy_value %3
/// ```
///
/// It's not needed to run this utility if SSAUpdater is used to create a _new_ OSSA liverange.
///
func replacePhisWithIncomingValues(phis: [Phi], _ context: some MutatingContext) {
  var currentPhis = phis
  // Do this in a loop because replacing one phi might open up the opportunity for another phi
  // and the order of phis in the array can be arbitrary.
  while true {
    var newPhis = [Phi]()
    for phi in currentPhis {
      if !replacePhiWithIncomingValue(phi: phi, context) {
        newPhis.append(phi)
      }
    }
    if newPhis.count == currentPhis.count {
      return
    }
    currentPhis = newPhis
  }
}

func registerPhiUpdater() {
  BridgedUtilities.registerPhiUpdater(
    // updateAllGuaranteedPhis
    { (bridgedCtxt: BridgedPassContext, bridgedFunction: BridgedFunction) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      let function = bridgedFunction.function;
      updateGuaranteedPhis(in: function, context)
    },
    // updateGuaranteedPhis
    { (bridgedCtxt: BridgedPassContext, bridgedPhiArray: BridgedArrayRef) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      var guaranteedPhis = Stack<Phi>(context)
      defer { guaranteedPhis.deinitialize() }
      bridgedPhiArray.withElements(ofType: BridgedValue.self) {
        for bridgedVal in $0 {
          let phi = Phi(bridgedVal.value)!
          if phi.value.ownership == .guaranteed {
            guaranteedPhis.append(phi)
          }
        }
      }
      updateGuaranteedPhis(phis: guaranteedPhis, context)
    },
    // replacePhisWithIncomingValues
    { (bridgedCtxt: BridgedPassContext, bridgedPhiArray: BridgedArrayRef) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      var phis = [Phi]()
      bridgedPhiArray.withElements(ofType: BridgedValue.self) {
        phis = $0.map { Phi($0.value)! }
      }
      replacePhisWithIncomingValues(phis: phis, context)
    }
  )
}

/// This pass is only used for testing.
/// In the regular pipeline it's not needed because optimization passes must make sure that borrowed-from
/// instructions are updated once the pass finishes.
let updateBorrowedFromPass = FunctionPass(name: "update-borrowed-from") {
  (function: Function, context: FunctionPassContext) in

  updateBorrowedFrom(in: function, context)
}
