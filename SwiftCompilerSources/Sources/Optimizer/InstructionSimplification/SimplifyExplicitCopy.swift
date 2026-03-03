//===--- SimplifyExplicitCopy.swift ---------------------------------------===//
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

// The `explicit_copy_value` and `explicit_copy_addr` instructions are only used for non-copyable
// diagnostics in the mandatory pipeline. After that we can replace them by their non-explicit counterparts
// so that optimizations (which only know of `copy_value` and `copy_addr`) can do their work.

/// Replaces
///
/// ```
///   %1 = explicit_copy_value %0
/// ```
/// ->
/// ```
///   %1 = copy_value %0
/// ```
///
extension ExplicitCopyValueInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if context.silStage == .raw {
      // Make sure we don't remove `explicit_copy_value` in the diagnostic pipeline.
      return
    }

    let builder = Builder(before: self, context)
    let copyValue = builder.createCopyValue(operand: fromValue)
    replace(with: copyValue, context)
  }
}

/// Replaces
///
/// ```
///   explicit_copy_addr %0 to %1
/// ```
/// ->
/// ```
///   copy_addr %0 to %1
/// ```
///
extension ExplicitCopyAddrInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if context.silStage == .raw {
      // Make sure we don't remove `explicit_copy_addr` in the diagnostic pipeline.
      return
    }

    let builder = Builder(before: self, context)
    builder.createCopyAddr(from: source, to: destination,
                           takeSource: isTakeOfSource, initializeDest: isInitializationOfDestination)
    context.erase(instruction: self)
  }
}
