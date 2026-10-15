//===--- Debugify.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Attaches a `debug_value` with a no-op debug reconstruction block to every
/// instruction result and block argument.
///
/// This is a stress tool for the debug info representation. It allows detecting
/// passes that mishandle `debug_value` instructions, by increasing the chances
/// of running into a type-chain verifier failure.
///
/// Enabled with `-Xllvm -sil-enable-debugify`.
let debugifyPass = FunctionPass(name: "debugify") {
  (function: Function, context: FunctionPassContext) in

  for block in function.blocks {
    // Function arguments already carry a variable from SILGen.
    if block != function.entryBlock {
      let builder = Builder(atBeginOf: block, context)
      for argument in block.arguments {
        builder.debugify(argument)
      }
    }

    for inst in block.instructions {
      // Existing debug_values all get a new no-op reconstruction block.
      if let debugValue = inst as? DebugValueInst {
        if debugValue.debugReconstructionBlock == nil, debugValue.operands.count == 1,
           !(debugValue.operands[0].value is Undef) {
          _ = debugValue.getOrCreateDebugReconstructionBlock()
        }
        continue
      }
      // No result, nothing to do.
      if inst.results.isEmpty {
        continue
      }

      // Some instructions don't support debug_values on their result.
      if inst is DropDeinitInst {
        continue
      }

      let builder = Builder(after: inst, context)
      for result in inst.results {
        builder.debugify(result)
      }
    }
  }
}

private extension Builder {
  /// Attaches a synthetic debug variable to `value`, if it can carry one.
  func debugify(_ value: Value) {
    // Ignore empty tuples.
    if value.type.isVoid {
      return
    }
    // Don't add a new debug_value if there's already one.
    if value.uses.contains(where: { $0.instruction is DebugValueInst }) {
      return
    }
    // Store borrow doesn't allow debug values on its destination.
    if value.uses.contains(where: { use in
      use.instruction is StoreBorrowInst && use.index == 1
    }) {
      return
    }
    // Returns nil for values that cannot carry a reconstruction block.
    createDebugifyDebugValue(value: value)
  }
}
