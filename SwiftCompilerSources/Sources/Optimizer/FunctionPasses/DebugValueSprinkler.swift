//===--- DebugValueSprinkler.swift ----------------------------------------===//
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
/// instruction result. Stress testing only.
///
/// This is a stress tool for the debug info representation. It allows detecting
/// passes that mishandle `debug_value` instructions, by increasing the chances
/// of running into a type-chain verifier failure.
///
/// Enabled with `-Xllvm -enable-debug-value-sprinkler`, in the optimized
/// pipeline only.
let debugValueSprinklerPass = FunctionPass(name: "debug-value-sprinkler") {
  (function: Function, context: FunctionPassContext) in
  
  // TODO: We could also handle phi nodes.

  for inst in function.instructions {
    // Existing debug_values all get a new no-op reconstruction block.
    if let debugValue = inst as? DebugValueInst {
      if debugValue.debugReconstructionBlock == nil, debugValue.operands.count == 1,
         !(debugValue.operands[0].value is Undef) {
        debugValue.getOrCreateDebugReconstructionBlock()
      }
      continue
    }
    // No result, nothing to do.
    if inst is TermInst || inst.results.isEmpty {
      continue
    }
    // Type-dependent operands are unsupported.
    // FIXME: Is this the correct way to check?
    if !inst.typeDependentOperands.isEmpty {
      continue
    }

    // Some instructions don't support debug_values on their result.
    if inst is DropDeinitInst {
      continue
    }

    let builder = Builder(after: inst, context)
    for result in inst.results {
      // Ignore empty tuples.
      if result.type.isVoid {
        continue
      }
      // Don't add a new debug_value if there's already one.
      if result.uses.contains(where: { $0.instruction is DebugValueInst }) {
        continue
      }
      // Store borrow doesn't allow debug values on its destination.
      if result.uses.contains(where: { use in
        use.instruction is StoreBorrowInst && use.index == 1
      }) {
        continue
      }
      // Returns nil for values that cannot carry a reconstruction block.
      builder.createSprinkledDebugValue(value: result)
    }
  }
}
