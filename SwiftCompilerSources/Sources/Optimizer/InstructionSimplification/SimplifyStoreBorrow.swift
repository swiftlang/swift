//===--- SimplifyStoreBorrow.swift ----------------------------------------===//
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

extension StoreBorrowInst : Simplifiable, SILCombineSimplifiable {

  func simplify(_ context: SimplifyContext) {
    if tryRemoveDead(context) {
      return
    }

    _ = tryForwardBorrowedAddress(context)
  }

  /// Remove a dead `store_borrow`
  ///
  /// ```
  ///   %1 = alloc_stack $T
  ///   %2 = store_borrow %0 to %1
  ///   end_borrow %2                // no other uses of %2 (except `debug_value`)
  ///   dealloc_stack %1
  /// ```
  /// ->
  /// ```
  ///   %1 = alloc_stack $T
  ///   dealloc_stack %1
  /// ```
  private func tryRemoveDead(_ context: SimplifyContext) -> Bool {

    guard uses.ignoreDebugUses.hasOnlyUsers(ofType: EndBorrowInst.self) else {
      return false
    }

    // "Move" `debug_value` instructions to the `store_borrow`'s destination operand, i.e. the `alloc_stack`
    // This allows salvageDebugInfo to correctly reconstruct the `debug_value` for the source operand.
    // ```
    //   %2 = store_borrow %1
    //   debug_value %2
    // ```
    // ->
    // ```
    //   %2 = store_borrow %1
    //   debug_value %1
    // ``
    for use in uses where use.instruction is DebugValueInst {
      use.set(to: destination, context)
    }

    context.erase(instructionIncludingAllUsers: self)
    return true
  }

  /// Replace a `store_borrow` of a `load_borrow` with the `load_borrow`'s
  /// address operand. Users of `store_borrow` cannot modify the addressed
  /// value, so this is safe, even though they are within `load_borrow`'s scope.
  ///
  /// ```
  ///   %3 = load_borrow %1
  ///   %4 = alloc_stack $T
  ///   %5 = store_borrow %3 to %4
  ///   // ... uses of %5
  ///   end_borrow %5
  ///   end_borrow %3
  ///   dealloc_stack %4           // id: %99
  /// ```
  /// ->
  /// ```
  ///   %3 = load_borrow %1
  ///   // ... uses of %1
  ///   end_borrow %3
  /// ```
  private func tryForwardBorrowedAddress(_ context: SimplifyContext) -> Bool {
    guard let loadBorrow = source.definingInstruction as? LoadBorrowInst
    else {
      return false
    }

    let originalAddress = loadBorrow.address

    context.erase(instructions: uses.users(ofType: EndBorrowInst.self))
    uses.replaceAll(with: originalAddress, context)
    context.erase(instructionIncludingAllUsers: allocStack)
    return true
  }
}
