//===--- SimplifyPartialApply.swift ---------------------------------------===//
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

extension PartialApplyInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if tryReplaceWithThinToThickFunction(context) {
      return
    }

    let keepForDebugInfo =
      context.preserveDebugInfo && uses.contains(where: { $0.instruction is DebugValueInst })

    let optimizedApplyOfPartialApply: Bool
    if keepForDebugInfo && !type.isImplicitlyDestroyable(in: parentFunction) {
      // Combining the calls of a closure that can't be destroyed implicitly is correct only if the
      // closure is then deleted. The combiner checks that the closure can be deleted, but it
      // expects debug uses to be salvaged. Here the closure is kept for its debug info instead, so
      // keep its calls.
      optimizedApplyOfPartialApply = false
    } else {
      optimizedApplyOfPartialApply = context.tryOptimizeApplyOfPartialApply(closure: self)
    }
    if optimizedApplyOfPartialApply {
      context.notifyInvalidatedStackNesting()
    }

    if keepForDebugInfo {
      return
    }

    // Try to delete the partial_apply.
    // In case it became dead because of tryOptimizeApplyOfPartialApply, we don't
    // need to copy all arguments again (to extend their lifetimes), because it
    // was already done in tryOptimizeApplyOfPartialApply.
    if context.tryDeleteDeadClosure(closure: self, needKeepArgsAlive: !optimizedApplyOfPartialApply) {
      context.notifyInvalidatedStackNesting()
    }
  }
}

private extension PartialApplyInst {
  /// A partial_apply without any substitutions or arguments is just a thin_to_thick_function.
  func tryReplaceWithThinToThickFunction(_ context: SimplifyContext) -> Bool {
    guard numArguments == 0,
          !hasSubstitutions,
          callee.type.functionTypeRepresentation == .thin,
          // Make sure the only difference is that the result is thick.
          type == callee.type.getFunctionType(withRepresentation: .thick)
                             .getFunctionType(withCalleeConvention: calleeConvention)
                             .getFunctionType(withNoEscape: type.isNoEscapeFunction)
    else {
      return false
    }
    let builder = Builder(before: self, context)
    let thinToThick = builder.createThinToThickFunction(thinFunction: callee, resultType: type)
    context.erase(instructions: uses.users(ofType: DeallocStackInst.self))
    replace(with: thinToThick, context)
    return true
  }
}
