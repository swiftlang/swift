//===--- SimplifyCopyBlock.swift ------------------------------------------===//
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

extension CopyBlockInst : Simplifiable, SILCombineSimplifiable {

  /// Removes a `copy_block` if its only uses, beside ownership instructions, are callees of function calls
  /// ```
  ///   %2 = copy_block %0
  ///   %3 = begin_borrow [lexical] %2
  ///   %4 = apply %3() : $@convention(block) @noescape () -> ()
  ///   end_borrow %3
  ///   destroy_value %2
  /// ```
  /// ->
  /// ```
  ///   %4 = apply %0() : $@convention(block) @noescape () -> ()
  /// ```
  ///
  func simplify(_ context: SimplifyContext) {
    if hasValidUses(block: self) {
      replaceBlock( self, with: operand.value, context)
      context.erase(instruction: self)
    }
  }
}

private func hasValidUses(block: Value) -> Bool {
  for use in block.uses {
    switch use.instruction {
    case let beginBorrow as BeginBorrowInst:
      if !hasValidUses(block: beginBorrow) {
        return false
      }
    case let apply as FullApplySite where apply.isCallee(operand: use):
      break
    case is EndBorrowInst, is DestroyValueInst:
      break
    default:
      return false
    }
  }
  return true
}

private func replaceBlock(_ block: Value, with original: Value, _ context: SimplifyContext) {
  for use in block.uses {
    switch use.instruction {
    case let beginBorrow as BeginBorrowInst:
      replaceBlock(beginBorrow, with: original, context)
      context.erase(instruction: beginBorrow)
    case is FullApplySite:
      use.set(to: original, context)
    case is EndBorrowInst, is DestroyValueInst:
      context.erase(instruction: use.instruction)
    default:
      fatalError("unhandled use")
    }
  }
}
