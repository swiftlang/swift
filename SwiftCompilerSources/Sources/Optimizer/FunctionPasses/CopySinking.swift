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
/// 2. Sinks copies over terminator instructions (switch_enum, checked_cast_br):
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
/// 3. Also handles `load [copy]` which is replaced by `load_borrow`:
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
/// The optimization can be done if:
/// * The `copy_value` or `load [copy]` has a single use.
/// * The source operand has guaranteed ownership.
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

  var changed = false

  for block in function.blocks {
    if !context.continueWithNextSubpassRun(for: block.terminator) {
      return
    }
    for arg in block.arguments {
      if let phi = Phi(arg) {
        if trySinkCopiesOverPhi(phi: phi, context) {
          changed = true
        }
      }
    }
    switch block.terminator {
    case let switchEnum as SwitchEnumInst:
      if trySinkCopiesOver(terminator: switchEnum, context) {
        changed = true
      }
    case let cast as CheckedCastBranchInst:
      guard cast.preservesReferenceCounts else {
        break
      }
      if trySinkCopiesOver(terminator: cast, context) {
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

private func trySinkCopiesOverPhi(phi: Phi, _ context: FunctionPassContext) -> Bool {
  for incomingOp in phi.incomingOperands {
    guard let copy = incomingOp.value as? CopyValueInst,
          copy.uses.isSingleUse,
          copy.fromValue.ownership == .guaranteed,
          isContainedInBorrowScope(from: copy, to: incomingOp.instruction, context)
    else {
      return false
    }
  }

  for incomingOp in phi.incomingOperands {
    let copy = incomingOp.value as! CopyValueInst
    copy.replace(with: copy.fromValue, context)
  }

  phi.value.set(ownership: .guaranteed, context)
  let newCopy = Builder(atBeginOf: phi.value.parentBlock, context).createCopyValue(operand: phi.value)
  phi.value.uses.ignore(user: newCopy).replaceAll(with: newCopy, context)

  return true
}

private func trySinkCopiesOver(terminator: ForwardingInstruction & UnaryInstruction & TermInst,
                               _ context: FunctionPassContext) -> Bool
{
  switch terminator.operand.value {
  case let copy as CopyValueInst:
    guard copy.uses.isSingleUse,
          copy.fromValue.ownership == .guaranteed,
          extendBorrowScope(from: copy, to: terminator, context)
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
          !mayWrite(toAddressOf: load, before: terminator, context)
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

private func isContainedInBorrowScope(from copy: CopyValueInst,
                                      to toInstruction: Instruction,
                                      _ context: FunctionPassContext) -> Bool
{
  var liveRange = InstructionRange(begin: copy, context)
  defer { liveRange.deinitialize() }

  liveRange.insert(toInstruction)
  return liveRange.isFullyContainedIn(scopeOf: copy.fromValue)
}


private func extendBorrowScope(from copy: CopyValueInst,
                               to terminator: TermInst,
                               _ context: FunctionPassContext) -> Bool
{
  var liveRange = InstructionRange(begin: copy, context)
  defer { liveRange.deinitialize() }

  liveRange.insert(contentsOf: terminator.successors.map { $0.instructions.first! })

  return extendBorrowScope(of: copy.fromValue, toOverlap: liveRange, context)
}

private func mayWrite(toAddressOf load: LoadInst,
                      before endInstruction: Instruction,
                      _ context: FunctionPassContext) -> Bool
{
  let aliasAnalysis = context.aliasAnalysis
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushPredecessors(of: endInstruction, ignoring: load)

  while let inst = worklist.pop() {
    if inst.mayWrite(toAddress: load.address, aliasAnalysis) {
      return true
    }
    worklist.pushPredecessors(of: inst, ignoring: load)
  }
  return false
}

