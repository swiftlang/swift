//===--- TrivialOwnershipElimination.swift ----------------------------------==//
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

let trivialOwnershipElimination = FunctionPass(name: "trivial-ownership-elimination") {
  (function: Function, context: FunctionPassContext) in
  eliminateTrivialOwnership(in: function, context)
}

private func eliminateTrivialOwnership(in function: Function, _ context: FunctionPassContext) {
  // Don't need to touch functions that are already non-OSSA, or which have
  // already had trivial ownership eliminated.
  guard function.hasOwnership, function.hasOwnershipForTrivialValues else {
    return
  }
  
  // Clear the ownership for trivial values flag. This way `isTrivial(in:
  // function)` tests will start revealing `true` for trivial types.
  function.set(hasOwnershipForTrivialValues: false, context)

  for block in function.blocks {
    // Remove ownership qualifiers from trivial basic block arguments.
    for arg in block.arguments where arg.type.isTrivial(in: function) {
      print(arg)
      arg.set(ownership: .none, context)
    }

    for insn in block.instructions {
      print(insn)
      // Change load/store/copy_addr qualifiers to trivial.
      switch insn {
      case let load as LoadInst where load.address.type.isTrivial(in: function):
        load.set(ownership: .trivial, context)

      case let store as StoreInst where store.destinationAddress.type.isTrivial(in: function):
        store.set(ownership: .trivial, context)

      case let ca as CopyAddrInst where ca.destinationAddress.type.isTrivial(in: function):
        ca.set(isTakeOfSource: false, context)
        ca.set(isInitializationOfDestination: false, context)

      // Replace load/store_borrow with trivial load/store.
      case let lb as LoadBorrowInst where lb.address.type.isTrivial(in: function):
        let builder = Builder(before: lb, context)
        let trivialLoad = builder.createLoad(fromAddress: lb.address, ownership: .trivial)
        lb.replace(with: trivialLoad, context)

      case let sb as StoreBorrowInst where sb.source.type.isTrivial(in: function):
        let builder = Builder(before: sb, context)
        builder.createStore(source: sb.source, destination: sb.destination, ownership: .trivial)
        sb.replace(with: sb.destination, context)

      // Eliminate destroy_value and destroy_addr.
      case let dv as DestroyValueInst where dv.destroyedValue.type.isTrivial(in: function):
        context.erase(instruction: dv)

      case let da as DestroyAddrInst where da.destroyedAddress.type.isTrivial(in: function):
        context.erase(instruction: da)

      // Replace `copy_value` with the original.
      case let cv as CopyValueInst where cv.fromValue.type.isTrivial(in: function):
        cv.replace(with: cv.fromValue, context)
      
      // Eliminate borrow scopes.
      case let bb as BeginBorrowInst where bb.borrowedValue.type.isTrivial(in: function):
        bb.replace(with: bb.borrowedValue, context)
      
      case let eb as EndBorrowInst where eb.borrow.type.isTrivial(in: function):
        context.erase(instruction: eb)

      // Leave other instructions alone.
      default:
        break
      }
    }
  }
}
