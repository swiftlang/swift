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
    if tryTransformThickToThinCallee(of: self, context) {
      return
    }
    if context.tryOptimizeKeypath(apply: self) {
      context.erase(instruction: self)
      return
    }
    _ = context.tryDevirtualize(apply: self, isMandatory: false)
  }
}

extension TryApplyInst : OnoneSimplifyable {
  func simplify(_ context: SimplifyContext) {
    _ = context.tryDevirtualize(apply: self, isMandatory: false)
  }
}

extension BeginApplyInst : OnoneSimplifyable {
  func simplify(_ context: SimplifyContext) {
    _ = context.tryDevirtualize(apply: self, isMandatory: false)
  }
}

/// Optimizes a thick function call if the callee is a `thin_to_thick_function` instruction:
///
///   %2 = thin_to_thick_function %1
///   %3 = apply %2(...) : @callee_guaranteed
/// ->
///   %2 = thin_to_thick_function %1
///   %3 = apply %1(...): @convention(thin)
///
private func tryTransformThickToThinCallee(of apply: ApplyInst, _ context: SimplifyContext) -> Bool {
  if let tttf = apply.callee as? ThinToThickFunctionInst,
     !apply.callee.type.isCalleeConsumedFunction
  {
    let builder = Builder(before: apply, context)
    let newApply = builder.createApply(function: tttf.operand.value,
                                       apply.substitutionMap,
                                       arguments: Array(apply.arguments),
                                       isNonThrowing: apply.isNonThrowing,
                                       isNonAsync: apply.isNonAsync,
                                       specializationInfo: apply.specializationInfo)
    apply.uses.replaceAll(with: newApply, context)
    context.erase(instruction: apply)
    return true
  }
  return false
}
