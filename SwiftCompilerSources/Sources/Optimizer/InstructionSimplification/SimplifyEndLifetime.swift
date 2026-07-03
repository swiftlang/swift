//===--- SimplifyEndLifetime.swift ----------------------------------------===//
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

extension EndLifetimeInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    // end_lifetime of undef is a no-op.
    if operand.value is Undef {
      context.erase(instruction: self)
      return
    }

    tryRemoveForwardingInstruction(context)
  }

  /// If the operand is produced by a forwarding instruction with no other uses,
  /// replace the end_lifetime with `end_lifetime`s on the forwarding instruction's
  /// owned operands and remove the forwarding instruction.
  ///
  /// ```
  ///   %3 = struct $S (%1, %2)
  ///   end_lifetime %3              // the only use of %3
  /// ```
  /// ->
  /// ```
  ///   end_lifetime %1
  ///   end_lifetime %2
  /// ```
  ///
  private func tryRemoveForwardingInstruction(_ context: SimplifyContext) {
    guard let fwdInst = operand.value as? (SingleValueInstruction & ForwardingInstruction),
          fwdInst.forwardingOwnership == .owned
    else {
      return
    }

    guard fwdInst.uses.ignoreDebugUses.hasOnlyUsers(ofType: EndLifetimeInst.self) else {
      return
    }

    let builder = Builder(before: fwdInst, context)
    for op in fwdInst.definedOperands where op.value.ownership == .owned && op.endsLifetime {
      builder.createEndLifetime(of: op.value)
    }

    context.erase(instructionIncludingAllUsers: fwdInst)
  }
}
