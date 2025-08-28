//===--- PhiUpdater.swift -------------------------------------------------===//
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

/// This pass is only used for testing.
/// In the regular pipeline it's not needed because optimization passes must make sure that borrowed-from
/// instructions are updated once the pass finishes.
let updateBorrowedFromPass = FunctionPass(name: "update-borrowed-from") {
  (function: Function, context: FunctionPassContext) in

  updateBorrowedFrom(in: function, context)
}
