//===--- SimplifyPhiArgument.swift ----------------------------------------===//
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

extension Phi {
  // Triggered from `BranchInst.simplify`
  func simplify(_ context: SimplifyContext) {
    if replacePhiWithIncomingValue(phi: self, context) {
      return
    }
    if replaceReborrowOfBeginBorrows(context) {
      return
    }
  }

  /// If `phi` is a re-borrow phi where all incoming operands are `begin_borrow`s of the same
  /// value, the re-borrow is redundant and can be replaced by a single `begin_borrow` of that
  /// value in the phi's block.
  ///
  /// ```
  ///   bb1:
  ///     %2 = begin_borrow %0
  ///     br bb3(%2)
  ///   bb2:
  ///     %3 = begin_borrow %0
  ///     br bb3(%3)
  ///   bb3(%4 : @reborrow $T):
  ///     %5 = borrowed %4 from (%0)
  ///     // ... uses of %5
  ///     end_borrow %5
  /// ```
  /// ->
  /// ```
  ///   bb1:
  ///     br bb3
  ///   bb2:
  ///     br bb3
  ///   bb3:
  ///     %5 = begin_borrow %0
  ///     // ... uses of %5
  ///     end_borrow %5
  /// ```
  private func replaceReborrowOfBeginBorrows(_ context: SimplifyContext) -> Bool {
    guard isReborrow,
          // All incoming operands must be `begin_borrow`s of the same value. As that value is borrowed
          // in every predecessor, it dominates the phi's block.
          let borrowedValue = getUniqueSourceOfIncomingBeginBorrows()
    else {
      return false
    }

    let block = value.parentBlock

    // Create a new borrow of the common value at the beginning of the phi's block and replace
    // the re-borrow (via its `borrowed_from` user) with it.
    let newBorrow = Builder(atBeginOf: block, context).createBeginBorrow(of: borrowedValue)
    let borrowedFrom = borrowedFrom!
    borrowedFrom.uses.replaceAll(with: newBorrow, context)
    context.erase(instruction: borrowedFrom)

    // Remove the phi operand from all predecessor branches and erase the now-dead incoming
    // `begin_borrow`s.
    erasePhiArgument(phi: self, erasingIncomingInstructions: true, context)
    return true
  }

  private func getUniqueSourceOfIncomingBeginBorrows() -> Value? {
    var borrowedValue: Value? = nil
    for incomingValue in incomingValues {
      guard let beginBorrow = incomingValue as? BeginBorrowInst,
            // The `begin_borrow`'s only purpose must be to feed the re-borrow.
            beginBorrow.uses.singleUse != nil
      else {
        return nil
      }
      if let borrowedValue {
        if beginBorrow.borrowedValue != borrowedValue {
          return nil
        }
      } else {
        borrowedValue = beginBorrow.borrowedValue
      }
    }
    return borrowedValue
  }
}
