//===--- SimplifyCondBranch.swift -----------------------------------------===//
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

extension CondBranchInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    tryConstantFold(context)
  }
}

private extension CondBranchInst {
  func tryConstantFold(_ context: SimplifyContext) {
    guard let literal = condition as? IntegerLiteralInst,
          let conditionValue = literal.value else
    {
      return
    }
    let builder = Builder(before: self, context)
    if conditionValue == 0 {
      builder.createBranch(to: falseBlock, arguments: Array(falseOperands.map { $0.value }))
    } else {
      builder.createBranch(to: trueBlock, arguments: Array(trueOperands.map { $0.value }))
    }
    context.erase(instruction: self)
  }
}
