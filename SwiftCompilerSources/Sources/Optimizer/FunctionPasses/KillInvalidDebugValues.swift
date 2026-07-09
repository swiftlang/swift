//===--- KillInvalidDebugValues.swift --------------------------------------===//
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

/// Kills `debug_value` instructions that reference a value after the underlying memory has been freed.
///
/// For each `debug_value`, the pass walks up through `begin_borrow` and guaranteed forwarding instructions
/// to find the root owned value. It then follows forwarding consumers (struct, tuple, etc.) transitively
/// (since those don't free the bits) until it reaches actual destroying consumers (`destroy_value`,
/// consuming `apply`, `store`). Forwarding consumes whose instruction is not a `ForwardingInstruction`
/// (e.g. `return`) are treated as range endpoints because the value leaves the function.
///
/// An `InstructionRange` from the root's definition to those transitive destroying consumers determines
/// the region where the bits are alive. Any `debug_value` outside this range has its operand killed.
///
/// This must run before OwnershipModelEliminator because it relies on ownership information.
let killInvalidDebugValuesPass = FunctionPass(name: "kill-invalid-debug-values") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else {
    return
  }

  // Collect debug_values grouped by root, so ranges are only computed once.
  var rootWorklist = ValueWorklist(context)
  defer { rootWorklist.deinitialize() }
  var debugValuesForRoot: [ObjectIdentifier: [DebugValueInst]] = [:]

  for block in function.blocks {
    for inst in block.instructions {
      guard let dbg = inst as? DebugValueInst,
            let root = findOwnershipRoot(of: dbg.operand.value) else {
        continue
      }
      rootWorklist.pushIfNotVisited(root)
      debugValuesForRoot[ObjectIdentifier(root), default: []].append(dbg)
    }
  }

  while let root = rootWorklist.pop() {
    let debugValues = debugValuesForRoot[ObjectIdentifier(root)]!
    guard context.continueWithNextSubpassRun(forValue: root) else {
      return
    }
    killInvalidDebugValues(debugValues, root: root, context)
  }
}

private func killInvalidDebugValues(_ debugValues: [DebugValueInst], root: Value,
                                    _ context: FunctionPassContext) {
  // Build a range from the root to where the content is actually destroyed.
  // Forwarding consumers (struct, tuple, etc.) don't free the content, so having debug values on
  // the original value is still fine: we follow through their results transitively until we reach
  // actual destroying consumers.
  var range = InstructionRange(for: root, context)
  defer { range.deinitialize() }

  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(root)

  while let current = worklist.pop() {
    for use in current.uses {
      if use.ownership == .destroyingConsume {
        range.insert(use.instruction)
      } else if use.ownership == .forwardingConsume {
        if let fwd = use.instruction as? ForwardingInstruction {
          for result in fwd.forwardedResults {
            worklist.pushIfNotVisited(result)
          }
        } else {
          // Treat non-forwarding instruction forwardingConsumes as an endpoint since we can't
          // follow further (return, throw, br, etc).
          range.insert(use.instruction)
        }
      }
    }
  }

  for dbg in debugValues {
    if !range.contains(dbg) {
      dbg.killOperand()
    }
  }
}

/// Walk up from a value through `begin_borrow` and guaranteed forwarding instructions to find the
/// owned value whose destroy frees the underlying memory.
///
/// Returns nil if no owned root can be found (e.g. guaranteed function arguments, `load_borrow`).
private func findOwnershipRoot(of value: Value) -> Value? {
  var current = value
  while true {
    if current.ownership == .owned {
      return current
    }
    if let bbi = current.definingInstruction as? BeginBorrowInst {
      current = bbi.borrowedValue
      continue
    }
    if let fwd = current.forwardingInstruction,
       current.ownership == .guaranteed,
       let singleOp = fwd.singleForwardedOperand {
      current = singleOp.value
      continue
    }
    return nil
  }
}
