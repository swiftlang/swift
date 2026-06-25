//===--- MergeBorrowScopes.swift -------------------------------------------==//
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

/// Merges to adjacent borrow scopes in a basic block.
///
/// ```
///   %2 = begin_borrow %1
///   use(%2)
///   end_borrow %2
///   ...
///   %6 = begin_borrow %1
///   use(%6)
///   end_borrow %6
///
/// ```
/// ->
/// ```
///   %2 = begin_borrow %1
///   use(%2)
///   ...
///   use(%2)
///   end_borrow %2
/// ```
///
/// This helps other optimizations, like common-subexpression-elimination, because the
/// borrow liveranges are larger and not split.
///
let mergeBorrowScopes = FunctionPass(name: "merge-borrow-scopes") {
    (function: Function, context: FunctionPassContext) in
  for block in function.blocks {
    doMergeBorrowScopes(in: block, context)
  }
}

private func doMergeBorrowScopes(in block: BasicBlock, _ context: FunctionPassContext) {
  var endBorrows = Dictionary<ObjectIdentifier, EndBorrowInst>()

  for inst in block.instructions {
    switch inst {
    case let endBorrow as EndBorrowInst:
      if let beginBorrow = endBorrow.borrow as? BeginBorrowInst {
        endBorrows[ObjectIdentifier(beginBorrow.borrowedValue)] = endBorrow
      }
    case let beginBorrow as BeginBorrowInst:
      if let endBorrow = endBorrows[ObjectIdentifier(beginBorrow.borrowedValue)] {
        endBorrows.removeValue(forKey: ObjectIdentifier(beginBorrow.borrowedValue))
        beginBorrow.replace(with: endBorrow.borrow, context)
        context.erase(instruction: endBorrow)
      }
    default:
      break
    }
  }
}
