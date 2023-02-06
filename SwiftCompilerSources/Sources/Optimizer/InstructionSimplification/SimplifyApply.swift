//===--- SimplifyApply.swift ----------------------------------------------===//
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

extension ApplyInst : OnoneSimplifyable {
  func simplify(_ context: SimplifyContext) {
    tryReplaceTrivialApplyOfPartialApply(context)
  }
}

private extension ApplyInst {
  func tryReplaceTrivialApplyOfPartialApply(_ context: SimplifyContext) {
    guard let pa = callee as? PartialApplyInst else {
      return
    }

    if pa.referencedFunction == nil {
      return
    }

    // Currently we don't handle generic closures. For Onone this is good enough.
    // TODO: handle it once we replace the SILCombine simplification with this.
    if !allArgumentsAreTrivial(arguments) {
      return
    }

    if !allArgumentsAreTrivial(pa.arguments) {
      return
    }

    if !substitutionMap.isEmpty {
      return
    }

    let allArgs = Array<Value>(arguments) + Array<Value>(pa.arguments)
    let builder = Builder(before: self, context)
    let newApply = builder.createApply(function: pa.callee, pa.substitutionMap, arguments: allArgs,
                                       isNonThrowing: isNonThrowing, isNonAsync: isNonAsync,
                                       specializationInfo: specializationInfo)
    uses.replaceAll(with: newApply, context)
    context.erase(instruction: self)

    if context.tryDeleteDeadClosure(closure: pa) {
      context.notifyInvalidatedStackNesting()
    }
  }
}

private func allArgumentsAreTrivial(_ args: LazyMapSequence<OperandArray, Value>) -> Bool {
  return !args.contains { !$0.hasTrivialType }
}
