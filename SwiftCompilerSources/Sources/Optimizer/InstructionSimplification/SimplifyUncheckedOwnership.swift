//===--- SimplifyUncheckedOwnership.swift ---------------------------------===//
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

extension UncheckedOwnershipInst : OnoneSimplifiable, SILCombineSimplifiable {

  /// Removes the ownership conversion if the operand already has the ownership of the result:
  /// ```
  ///   %1 = unchecked_ref_cast %0                  // ownership: none
  ///   %2 = unchecked_ownership %1, forwarding: @none
  ///   // ... uses of %2
  /// ```
  /// ->
  /// ```
  ///   %1 = unchecked_ref_cast %0                  // ownership: none
  ///   // ... uses of %1
  /// ```
  ///
  /// This can happen after the ownership of the operand became "none", e.g. because
  /// `canonicalizeOwnershipToNone` updated the forwarding ownership of the operand's definition.
  func simplify(_ context: SimplifyContext) {
    if operand.value.ownership == ownership {
      replace(with: operand.value, context)
    }
  }
}
