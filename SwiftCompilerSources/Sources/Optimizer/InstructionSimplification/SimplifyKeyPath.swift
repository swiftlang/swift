//===--- SimplifyKeyPath.swift --------------------------------------------===//
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

extension KeyPathInst : OnoneSimplifyable {
  func simplify(_ context: SimplifyContext) {
    if allUsesRemovable(instruction: self) {
      if parentFunction.hasOwnership {
        let builder = Builder(after: self, context)
        for operand in self.operands {
          if !operand.value.type.isTrivial(in: parentFunction) {
            builder.createDestroyValue(operand: operand.value)
          }
        }
      }
      context.erase(instructionIncludingAllUsers: self)
    }
  }
}

fileprivate func allUsesRemovable(instruction: Instruction) -> Bool {
  for result in instruction.results {
    for use in result.uses {
      if !(use.instruction is UpcastInst || use.instruction is DestroyValueInst || use.instruction is BeginBorrowInst || use.instruction is EndBorrowInst) {
        return false
      }
      if !allUsesRemovable(instruction: use.instruction) {
        return false
      }
    }
  }
  return true
}
