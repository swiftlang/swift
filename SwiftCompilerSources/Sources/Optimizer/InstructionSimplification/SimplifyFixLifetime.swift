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

extension FixLifetimeInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if tryReplaceWithLoadFromAllocStack(context) {
      return
    }

    _ = tryRemove(context)
  }

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
  private func tryReplaceWithLoadFromAllocStack(_ context: SimplifyContext) -> Bool {
    let opValue = operand.value
    guard opValue is AllocStackInst || opValue is StoreBorrowInst,
          opValue.type.isLoadable(in: parentFunction)
    else {
      return false
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
    return true
  }

  /// Removes a `fix_lifetime` if the underlying object is immortal, anyway.
  /// For example, the swift empty array storage.
  /// ```
  ///   %0 = global_addr @emptyArrayStorage : $*EmptyArrayStorage
  ///   %1 = address_to_pointer [stack_protection] %0 to $Builtin.RawPointer
  ///   %2 = raw_pointer_to_ref %1 to $ArrayStorage
  ///   %3 = copy_value %2
  ///   fix_lifetime %3
  /// ```
  ///
  private func tryRemove(_ context: SimplifyContext) -> Bool {
    var op = operand.value

    while true {
      switch op {
      case is GlobalAddrInst:
        context.erase(instruction: self)
        return true
      case is RawPointerToRefInst, is AddressToPointerInst, is CopyValueInst:
        op = (op as! UnaryInstruction).operand.value
      default:
        return false
      }
    }
  }
}

