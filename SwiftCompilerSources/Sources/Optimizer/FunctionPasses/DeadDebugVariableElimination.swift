//===--- DeadDebugVariableElimination.swift --------------------------------===//
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

/// Eliminates `debug_value` instructions whose scope is dead.
///
/// After optimization, all non-debug instructions from a scope may be eliminated,
/// but `debug_value` instructions remain. Since there's no breakpoint location left in that
/// scope, these debug values can never be observed by the debugger. Removing them saves memory.
let deadDebugVariableEliminationPass = FunctionPass(name: "dead-debug-variable-elimination") {
  (function: Function, context: FunctionPassContext) in

  // Phase 1: Build the set of live scopes.
  // A scope is live if it is visible from setting a breakpoint on a real instruction.
  var liveScopes = Set<DebugScope>()

  for inst in function.instructions {
    if inst is MetaInstruction {
      // Breakpoints cannot be set on meta instructions.
      continue
    }
    guard let scope = inst.location.scope else {
      continue
    }

    // Each parent scope and parent frame is live.
    var currentFrame: DebugScope? = scope
    while let frame = currentFrame {
      var currentScope: DebugScope? = frame
      while let scope = currentScope {
        guard liveScopes.insert(scope).inserted else {
          break // Parents were already inserted.
        }
        currentScope = scope.parentScope
      }
      currentFrame = frame.inlinedCallSite
    }
  }

  // Phase 2: Remove debug_values whose variable's scope is not live.
  for case let inst as DebugValueInst in function.instructions {
    if let scope = inst.debugVariable?.scope, liveScopes.contains(scope) {
      // Keep live scopes.
      continue
    }
    context.erase(instruction: inst, salvageDebugInfo: false)
  }
}
