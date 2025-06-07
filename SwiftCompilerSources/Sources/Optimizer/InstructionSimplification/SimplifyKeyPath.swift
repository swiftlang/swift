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

extension KeyPathInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if allUsesRemovable(instruction: self) {
      if parentFunction.hasOwnership {
        let builder = Builder(after: self, context)
        for operand in self.operands {
          if !operand.value.type.isTrivial(in: parentFunction) {
            if operand.value.type.isAddress {
              builder.createDestroyAddr(address: operand.value)
            } else {
              builder.createDestroyValue(operand: operand.value)
            }
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
      switch use.instruction {
      case is UpcastInst,
           is DestroyValueInst,
           is StrongReleaseInst,
           is BeginBorrowInst,
           is EndBorrowInst,
           is MoveValueInst,
           is CopyValueInst:
        // This is a removable instruction type, continue descending into uses
        if !allUsesRemovable(instruction: use.instruction) {
          return false
        }

      default:
        return false
      }
    }
  }
  return true
}
