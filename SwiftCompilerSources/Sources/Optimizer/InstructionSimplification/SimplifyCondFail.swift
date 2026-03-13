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

    if context.options.shouldRemoveCondFail(withMessage: message, inFunction: parentFunction.name) {
      context.erase(instruction: self)
      return
    }

    guard let literal = condition as? IntegerLiteralInst,
          let value = literal.value
    else {
      return
    }

    if value == 0 {
      // Eliminates
      // ```
      //   %0 = integer_literal 0
      //   cond_fail %0, "message"
      // ```
      context.erase(instruction: self)
    } else {

      // Inserts an unreachable after an unconditional fail:
      // ```
      //   %0 = integer_literal 1
      //   cond_fail %0, "message"
      //   // following instructions
      // ```
      // ->
      // ```
      //   %0 = integer_literal 1
      //   cond_fail %0, "message"
      //   unreachable
      // deadblock:
      //   // following instructions
      // ```
      if InstructionList(first: self.next).allSatisfy({ $0.isUnreachableOrEndingLifetime }) {
        // Don't remove instructions which would be re-inserted by lifetime completion.
        return
      }
      let builder = Builder(after: self, context)
      let unreachable = builder.createUnreachable()
      _ = context.splitBlock(after: unreachable)
    }
  }
}

private extension Instruction {
  var isUnreachableOrEndingLifetime: Bool {
    switch self {
    case is EndBorrowInst, is DestroyValueInst, is EndLifetimeInst, is DeallocStackInst, is EndAccessInst,
         is UnreachableInst:
      return true
    default:
      return false
    }
  }
}
