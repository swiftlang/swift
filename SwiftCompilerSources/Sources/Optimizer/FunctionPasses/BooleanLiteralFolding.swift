//===--- BooleanLiteralFolding.swift ---------------------------------------==//
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

/// Constant folds conditional branches with boolean literals as operands.
///
/// ```
///   %1 = integer_literal -1
///   %2 = apply %bool_init(%1)   // Bool.init(_builtinBooleanLiteral:)
///   %3 = struct_extract %2, #Bool._value
///   cond_br %3, bb1, bb2
/// ```
/// ->
/// ```
///   ...
///   br bb1
/// ```
///
/// This pass is intended to run before DefiniteInitialization, where mandatory inlining and
/// constant folding didn't run, yet (which would perform this kind of optimization).
///
/// This optimization is required to let DefiniteInitialization handle boolean literals correctly.
/// For example in infinite loops:
///
/// ```
///    init() {
///      while true {           // DI need to know that there is no loop exit from this while-statement
///        if some_condition {
///          member_field = init_value
///          break
///        }
///      }
///    }
/// ```
///
let booleanLiteralFolding = FunctionPass(name: "boolean-literal-folding") {
  (function: Function, context: FunctionPassContext) in

  var changed = false
  for block in function.blocks {
    if let condBr = block.terminator as? CondBranchInst {
      changed = fold(condBranch: condBr, context) || changed
    }
  }
  if changed {
    _ = context.removeDeadBlocks(in: function)
  }
}

private func fold(condBranch: CondBranchInst, _ context: FunctionPassContext) -> Bool {
  guard let structExtract = condBranch.condition as? StructExtractInst,
        let initApply = structExtract.struct as? ApplyInst,
        initApply.hasSemanticsAttribute("bool.literal_init"),
        initApply.arguments.count == 2,
        let literal = initApply.arguments[0] as? IntegerLiteralInst,
        let literalValue = literal.value else
  {
    return false
  }

  let builder = Builder(before: condBranch, context)
  builder.createBranch(to: literalValue == 0 ? condBranch.falseBlock : condBranch.trueBlock)
  context.erase(instruction: condBranch)
  return true
}
