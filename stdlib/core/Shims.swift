//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Additions to 'SwiftShims' that can be written in Swift.
///
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
import SwiftShims

internal func _makeSwiftNSFastEnumerationState()
   -> _SwiftNSFastEnumerationState {
  return _SwiftNSFastEnumerationState(
    state: 0, itemsPtr: nil, mutationsPtr: nil,
    extra: (0, 0, 0, 0, 0))
}

/// A dummy value that is be used as the target for `mutationsPtr` in fast
/// enumeration implementations.
var _fastEnumerationStorageMutationsTarget: CUnsignedLong = 0

/// A dummy pointer to be used as `mutationsPtr` in fast enumeration
/// implementations.
public // @testable
var _fastEnumerationStorageMutationsPtr: UnsafeMutablePointer<CUnsignedLong> {
  return UnsafeMutablePointer(
      Builtin.addressof(&_fastEnumerationStorageMutationsTarget))
}

internal func _isUniquelyReferenced_native(
  inout x: Builtin.NativeObject
) -> Bool {
  let p = UnsafePointer<_HeapObject>(Builtin.bridgeToRawPointer(x))
  let result = _swift_isUniquelyReferenced_nonNull_native(p)
  Builtin.fixLifetime(x)
  return result
}
#endif
