//===--- SimplifyMoveValue.swift ------------------------------------------===//
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

/// The `move_value` instruction is only used to specify flags.
/// Remove a `move_value` which either doesn't specify any flags or which flags are not relevant
/// outside the mandatory pipeline, like `[lexical]`.
///
/// Replaces
///
/// ```
///   %1 = move_value %0
///   use %1
/// ```
/// ->
/// ```
///   use %0
/// ```
///
extension MoveValueInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if hasPointerEscape {
      // Keep this flag.
      return
    }
    replace(with: fromValue, context)
  }
}
