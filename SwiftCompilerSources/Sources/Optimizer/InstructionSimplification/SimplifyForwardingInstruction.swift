//===--- SimplifyForwardingInstruction.swift ------------------------------===//
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

extension ForwardingInstruction {
  /// Simplifications which apply to all forwarding instructions, regardless of their kind.
  ///
  /// This is called by the simplification passes for every forwarding instruction, in addition to
  /// the `simplify` of the concrete instruction class (if it has one).
  func simplifyForwardingInstruction(_ context: SimplifyContext) {
    canonicalizeOwnershipToNone(context)
  }

  /// If none of the forwarded operands has ownership anymore, the forwarding ownership must be
  /// updated to `.none`:
  /// ```
  ///   %1 = unchecked_ref_cast %0                          // ownership: none
  ///   %2 = struct $S (%1), forwarding: @owned              // still owned!
  ///   destroy_value %2
  /// ```
  /// ->
  /// ```
  ///   %1 = unchecked_ref_cast %0                          // ownership: none
  ///   %2 = struct $S (%1)                                 // ownership: none
  ///   destroy_value %2                                    // removed by SimplifyDestroyValue
  /// ```
  ///
  /// The forwarding ownership is stored in the instruction and therefore becomes stale when an
  /// owned operand is replaced by a value without ownership. That happens e.g. when the ObjectOutliner
  /// replaces an `alloc_ref` with a `global_value` or a `copy_value` of an immortal reference is
  /// removed.
  ///
  /// This is run for all forwarding instructions by the simplification passes, so that the `.none`
  /// ownership is propagated down a forwarding chain by the pass' worklist.
  ///
  private func canonicalizeOwnershipToNone(_ context: SimplifyContext) {
    guard forwardingOwnership != .none,
          // A terminator forwards to block arguments, which have their own ownership kind.
          !(self is TermInst),
          // Move-only values are always owned, even if all their operands are trivial (see
          // `ValueOwnershipKind::forwardToInit`). E.g. when such an instruction is cloned, the
          // SILBuilder re-creates it with owned forwarding ownership.
          !results.contains(where: { $0.type.isMoveOnly }),
          !forwardedOperands.isEmpty,
          forwardedOperands.allSatisfy({ $0.value.ownership == .none })
    else {
      return
    }
    setForwardingOwnership(to: .none, context)
    // Re-simplify users which are located before this instruction. Users which come afterwards are
    // visited by the worklist anyway.
    for result in results {
      for use in result.uses {
        context.notifyInstructionChanged(use.instruction)
      }
    }
  }
}
