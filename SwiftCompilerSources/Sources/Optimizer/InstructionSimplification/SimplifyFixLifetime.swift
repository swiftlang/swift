//===--- SimplifyFixLifetime.swift ----------------------------------------===//
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

/// Canonicalize a `fix_lifetime` from an address to a `load` + `fix_lifetime`:
/// ```
///    %1 = alloc_stack $T
///    ...
///    fix_lifetime %1
/// ```
/// ->
/// ```
///    %1 = alloc_stack $T
///    ...
///    %2 = load %1
///    fix_lifetime %2
/// ```
///
/// This transformation is done for `alloc_stack` and `store_borrow` (which always has an `alloc_stack`
/// operand).
/// The benefit of this transformation is that it enables other optimizations, like mem2reg.
///
extension FixLifetimeInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    let opValue = operand.value
    guard opValue is AllocStackInst || opValue is StoreBorrowInst,
          opValue.type.isLoadable(in: parentFunction)
    else {
      return
    }

    let builder = Builder(before: self, context)
    let loadedValue: Value
    if !parentFunction.hasOwnership {
      loadedValue = builder.createLoad(fromAddress: opValue, ownership: .unqualified)
    } else if opValue.type.isTrivial(in: parentFunction) {
      loadedValue = builder.createLoad(fromAddress: opValue, ownership: .trivial)
    } else {
      loadedValue = builder.createLoadBorrow(fromAddress: opValue)
      Builder(after: self, context).createEndBorrow(of: loadedValue)
    }
    operand.set(to: loadedValue, context)
  }
}

