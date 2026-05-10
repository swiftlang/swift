//===--- CondFailOptimization.swift ----------------------------------------==//
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

/// This optimization handles unconditional `cond_fail` instructions, i.e. `cond_fail`s with a non-zero
/// `integer_literal` operand.
/// It cuts off the control flow after such a `cond_fail` by inserting an `unreachable` instruction.
/// The resulting dead code - instructions and blocks - gets removed.
///
/// ```
///   %0 = integer_literal 1
///   cond_fail %0, "message"
///   // following instructions
/// ```
/// ->
/// ```
///   %0 = integer_literal 1
///   cond_fail %0, "message"
///   unreachable
/// ```
///
/// This optimization cannot be done as instruction simplification, because it can leave OSSA
/// lifetimes uncompleted. Other simplification may depend on complete lifetimes.
/// This pass runs lifetime completion after optimizing all `cond_fail`s.
///
let condFailOptimization = FunctionPass(name: "cond-fail-optimization") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    if let cfi = inst as? CondFailInst {
      tryRemoveUnconditional(condFail: cfi, context)
    }
  }

  _ = context.removeDeadBlocks(in: function)

  if context.needBreakInfiniteLoops {
    breakInfiniteLoops(in: function, context)
  }

  if context.needCompleteLifetimes {
    completeLifetimes(in: function, context)
  }
}

private func tryRemoveUnconditional(condFail: CondFailInst, _ context: FunctionPassContext) {
  guard let literal = condFail.condition as? IntegerLiteralInst,
        let value = literal.value,
        value != 0
  else {
    return
  }

  if InstructionList(first: condFail.next).allSatisfy({ $0.isUnreachableOrEndingLifetime }) {
    // Don't remove instructions which would be re-inserted by lifetime completion.
    return
  }
  let builder = Builder(after: condFail, context)
  let unreachable = builder.createUnreachable()
  _ = context.splitBlock(after: unreachable)
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
