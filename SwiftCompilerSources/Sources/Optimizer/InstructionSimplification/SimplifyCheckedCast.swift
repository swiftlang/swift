//===--- SimplifyCheckedCast.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension CheckedCastAddrBranchInst : OnoneSimplifyable {
  func simplify(_ context: SimplifyContext) {
    guard let castWillSucceed = self.dynamicCastResult else {
      return
    }
    if castWillSucceed {
      // TODO: handle cases where the operand address types are different.
      if source.type == destination.type {
        replaceSuccess(context)
      }
    } else {
      replaceFailure(context)
    }
  }
}

private extension CheckedCastAddrBranchInst {
  func replaceSuccess(_ context: SimplifyContext) {
    let builder = Builder(before: self, context)
    switch consumptionKind {
    case .TakeAlways, .TakeOnSuccess:
      builder.createCopyAddr(from: source, to: destination, takeSource: true, initializeDest: true)
    case .CopyOnSuccess:
      builder.createCopyAddr(from: source, to: destination, takeSource: false, initializeDest: true)
    }
    builder.createBranch(to: successBlock)
    context.erase(instruction: self)
  }

  func replaceFailure(_ context: SimplifyContext) {
    let builder = Builder(before: self, context)
    switch consumptionKind {
    case .TakeAlways:
      builder.createDestroyAddr(address: source)
    case .CopyOnSuccess, .TakeOnSuccess:
      break
    }
    builder.createBranch(to: failureBlock)
    context.erase(instruction: self)
  }
}
