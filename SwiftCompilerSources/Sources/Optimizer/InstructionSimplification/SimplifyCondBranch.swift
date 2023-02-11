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

extension CondBranchInst : OnoneSimplifyable {
  func simplify(_ context: SimplifyContext) {
    tryConstantFold(context)
  }
}

private extension CondBranchInst {
  func tryConstantFold(_ context: SimplifyContext) {
    guard let literal = condition as? IntegerLiteralInst else {
      return
    }
    let builder = Builder(before: self, context)
    if literal.value.isZero() {
      builder.createBranch(to: falseBlock, arguments: Array(falseOperands.map { $0.value }))
    } else {
      builder.createBranch(to: trueBlock, arguments: Array(trueOperands.map { $0.value }))
    }
    context.erase(instruction: self)
  }
}
