//===--------- DiagnoseUnknownConstValues.swift --------------------------===//
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
import AST

/// Performs mandatory diagnostic pass for emitting errors for '@const' values which the compiler was not able to
/// simplify/interpret/reduce to a symbolic value.
///
let diagnoseUnknownConstValues = ModulePass(name: "diagnose-unknown-const-values") {
  (moduleContext: ModulePassContext) in
  var constExprEvaluator: ConstExpressionEvaluator = .init()
  defer { constExprEvaluator.deinitialize() }
  
  // Verify all const globals to be initialized with compile-time known values
  verifyGlobals(moduleContext)
  
  // Verify @const lets appearing as local variables
  verifyLocals(constExprEvaluator: &constExprEvaluator,
               moduleContext)
  
  // For each function call, ensure arguments to @const parameters are all compile-time known values
  verifyCallArguments(constExprEvaluator: &constExprEvaluator,
                      moduleContext)
  
  // For each `@const` function, ensure it is fully evaluable/interpretable at compile time
  verifyFunctions(moduleContext)
}

private func verifyGlobals(_ context: ModulePassContext) {
  for gv in context.globalVariables where gv.isConst {
    if gv.staticInitValue == nil {
      context.diagnosticEngine.diagnose(.require_const_initializer_for_const,
                                        at: gv.varDecl?.location.sourceLoc)
    }
  }
}

private func verifyLocals(constExprEvaluator: inout ConstExpressionEvaluator,
                          _ context: ModulePassContext) {
  for f in context.functions {
    for i in f.instructions {
      if let dbi = i as? DebugValueInst {
        verifyLocal(debugValueInst: dbi, constExprState: &constExprEvaluator, in: context)
      }
    }
  }
}

private func verifyLocal(debugValueInst: DebugValueInst,
                         constExprState: inout ConstExpressionEvaluator,
                         in context: ModulePassContext) {
  guard let localVarDecl = debugValueInst.varDecl,
        !(localVarDecl is ParamDecl),
        localVarDecl.isConst else {
    return
  }
  
  if !constExprState.isConstantValue(debugValueInst.operand.value) {
    context.diagnosticEngine.diagnose(.require_const_initializer_for_const,
                                      at: debugValueInst.location)
  }
}

private func verifyCallArguments(constExprEvaluator: inout ConstExpressionEvaluator,
                                 _ context: ModulePassContext) {
  for f in context.functions {
    for i in f.instructions {
      // TODO: Consider closures (partial_apply)
      if let apply = i as? FullApplySite {
        verifyCallArguments(apply: apply, constExprState: &constExprEvaluator, in: context)
      }
    }
  }
}

private func verifyCallArguments(apply: FullApplySite,
                                 constExprState: inout ConstExpressionEvaluator,
                                 in context: ModulePassContext) {
  guard let calleeFn = apply.referencedFunction else {
    return
  }
  for (paramIdx, param) in calleeFn.convention.parameters.enumerated() where param.hasOption(.const) {
    let matchingOperand = apply.parameterOperands[paramIdx]
    if !constExprState.isConstantValue(matchingOperand.value) {
      context.diagnosticEngine.diagnose(.require_const_arg_for_parameter,
                                        at: apply.location)
    }
  }
}

private func verifyFunctions(_ context: ModulePassContext) {
  // TODO: Implement
}
