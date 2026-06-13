//===--- SimplifyUncheckedOwnershipConversion.swift -----------------------===//
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

import SIL

extension UncheckedOwnershipConversionInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {

    // Replaces a sequence which is commonly found in generated class destructors:
    //
    // ```
    //   %1 = some_owned_value
    //   %2 = begin_borrow %1                // the only use of %1
    //   %3 = unchecked_ref_cast %2 to $C
    //   %4 = unchecked_ownership_conversion %3, @guaranteed to @owned
    //   end_borrow %2
    //   end_lifetime %1
    // ```
    // ->
    // ```
    //   %1 = some_owned_value
    //   %2 = begin_borrow %1                // now dead
    //   end_borrow %2
    //   %4 = unchecked_ref_cast %1 to $C
    // ```
    //
    guard self.ownership == .owned,
          let refCast = operand.value as? UncheckedRefCastInst,
          let beginBorrow = refCast.fromInstance as? BeginBorrowInst,
          beginBorrow.borrowedValue.ownership == .owned,
          let endBorrowInst = beginBorrow.endInstructions.singleElement,
          endBorrowInst is EndBorrowInst,
          beginBorrow.borrowedValue.uses.ignore(user: beginBorrow).ignoreDebugUses.hasOnlyUsers(ofType: EndLifetimeInst.self),
          self.parentBlock == endBorrowInst.parentBlock,
          self.hasNoUsers(before: endBorrowInst)
    else {
      return
    }

    let builder = Builder(after: endBorrowInst, context)
    let newRefCast = builder.createUncheckedRefCast(from: beginBorrow.borrowedValue, to: refCast.type)
    self.replace(with: newRefCast, context)
    context.erase(instruction: refCast)
    context.erase(instructions: beginBorrow.borrowedValue.uses.users(ofType: EndLifetimeInst.self))
  }

  private func hasNoUsers(before otherInst: Instruction) -> Bool {
    for inst in InstructionList(first: self.next) {
      if inst == otherInst {
        return true
      }
      if inst.operands.contains(where: { $0.value == self }) {
        return false
      }
    }
    fatalError("end_borrow expected to be in same block as unchecked_ownership_conversion")
  }
}
