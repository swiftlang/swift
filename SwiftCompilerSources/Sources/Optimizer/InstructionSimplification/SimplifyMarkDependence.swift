//===--- SimplifyMarkDependence.swift -------------------------------------===//
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

// Note: this simplification cannot run before dependency diagnostics.
// See `var isRedundant` below.

extension MarkDependenceInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if isRedundant ||
       // A literal lives forever, so no mark_dependence is needed.
       // This pattern can occur after StringOptimization when a utf8CString of a literal is replaced
       // by the string_literal itself.
       value.isLiteral
    {
      replace(with: value, context)
      return
    }
    simplifyBaseOperand(context)
  }
}

extension MarkDependenceAddrInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if isRedundant {
      context.erase(instruction: self)
      return
    }
    simplifyBaseOperand(context)
  }
}

private extension MarkDependenceInstruction {
  var isRedundant: Bool {
    if base.type.isObject && base.type.isTrivial(in: base.parentFunction) {
      // Sometimes due to specialization/builtins, we can get a mark_dependence whose base is a trivial
      // typed object. Trivial values live forever. Therefore the mark_dependence does not have a meaning.
      // Note: the mark_dependence is still needed for lifetime diagnostics. So it's important that this
      //       simplification does not run before the lifetime diagnostic pass.
      return true
    }
    // If the value is an address projection from the base the mark_dependence is not needed because the
    // base cannot be destroyed before the accessing the value, anyway.
    if valueOrAddress.type.isAddress, base.type.isAddress,
       // But we still need to keep the mark_dependence for non-escapable types because a non-escapable
       // value can be copied and copies must not outlive the base.
       valueOrAddress.type.isEscapable(in: parentFunction),
       base.accessPath.isEqualOrContains(valueOrAddress.accessPath)
    {
      return true
    }
    return false
  }

  func simplifyBaseOperand(_ context: SimplifyContext) {
    /// In OSSA, the `base` is a borrow introducing operand. It is pretty complicated to change the base.
    /// So, for simplicity, we only do this optimization when OSSA is already lowered.
    if parentFunction.hasOwnership {
      return
    }
    // Replace the base operand with the operand of the base value if it's a certain kind of forwarding
    // instruction.
    let rootBase = base.lookThroughEnumAndExistentialRef
    if rootBase != base {
      baseOperand.set(to: rootBase, context)
    }
  }
}

private extension Value {
  /// True, if this is a literal instruction or a struct of a literal instruction.
  /// What we want to catch here is a `UnsafePointer<Int8>` of a string literal.
  var isLiteral: Bool {
    switch self {
    case let s as StructInst:
      if let singleOperand = s.operands.singleElement {
        return singleOperand.value.isLiteral
      }
      return false
    case  is IntegerLiteralInst, is FloatLiteralInst, is StringLiteralInst:
      return true
    default:
      return false
    }
  }

  var lookThroughEnumAndExistentialRef: Value {
    switch self {
    case let e as EnumInst:
      if let payload = e.payload {
        return payload.lookThroughEnumAndExistentialRef
      }
      return self
    case let ier as InitExistentialRefInst:
      return ier.instance.lookThroughEnumAndExistentialRef
    case let oer as OpenExistentialRefInst:
      return oer.existential.lookThroughEnumAndExistentialRef
    default:
      return self
    }
  }
}
