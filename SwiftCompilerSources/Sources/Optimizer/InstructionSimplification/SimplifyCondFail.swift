//===--- SimplifyCondFail.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension CondFailInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {

    if let literal = condition as? IntegerLiteralInst, let value = literal.value {
      if value == 0 {
        // Eliminates
        // ```
        //   %0 = integer_literal 0
        //   cond_fail %0, "message"
        // ```
        context.erase(instruction: self)
      }
      // Even if `shouldRemoveCondFail` is true, we don't want to remove unconditioal fails,
      // i.e. `cond_fail` with a non-zero condition, because this would prevent removing dead
      // code after such a `cond_fail` in a later optimization.
      return
    }

    if context.options.shouldRemoveCondFail(withMessage: message, inFunction: parentFunction.name) {
      context.erase(instruction: self)
    }
  }
}
