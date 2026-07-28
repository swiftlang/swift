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

    /// Remove a dead `store_borrow`
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

    guard uses.ignoreDebugUses.hasOnlyUsers(ofType: EndBorrowInst.self) else {
      return
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
    for debugValue in uses.users(ofType: DebugValueInst.self) {
      debugValue.operand.set(to: destination, context)
    }

    context.erase(instructionIncludingAllUsers: self)
  }
}
