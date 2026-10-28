//===--- SimplifyRawPointerToRef.swift ------------------------------------===//
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

extension RawPointerToRefInst : OnoneSimplifiable, SILCombineSimplifiable {

  /// Sets the `immortal` flag if used for an Array/Set/Dictionary singleton:
  /// ```
  ///   %0 = global_addr @_swiftEmptyArrayStorage
  ///   %1 = address_to_pointer %0
  ///   %2 = raw_pointer_to_ref %1
  /// ```
  /// ->
  /// ```
  ///   %0 = global_addr @_swiftEmptyArrayStorage
  ///   %1 = address_to_pointer %0
  ///   %2 = raw_pointer_to_ref [immortal] %1
  /// ```
  ///
  /// `raw_pointer_to_ref` is only used to implement the `bridgeFromRawPointer`
  /// builtin, which in turn is only used to create the empty COW buffer
  /// singletons (Array, Set, Dictionary) and for the `UnsafeCurrentTask._task`
  /// ABI-compat shim. Since Swift 5.1 those statically allocated objects have
  /// "immortal" reference counts, so retaining and releasing them is a no-op.
  /// This is only true if the Swift 5.1 runtime is available on the deployment
  /// target.
  func simplify(_ context: SimplifyContext) {
    if isImmortal {
      return
    }
    if parentFunction.isSwift51RuntimeAvailable,
       let atp = pointer as? AddressToPointerInst,
       atp.address is GlobalAddrInst
    {
      set(isImmortal: true, context)
    }
  }
}
