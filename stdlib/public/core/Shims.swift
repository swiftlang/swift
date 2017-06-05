//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Additions to 'SwiftShims' that can be written in Swift.
///
//===----------------------------------------------------------------------===//

#if _runtime(_ObjC)
import SwiftShims

@_inlineable
@_versioned
internal func _makeSwiftNSFastEnumerationState()
   -> _SwiftNSFastEnumerationState {
  return _SwiftNSFastEnumerationState(
    state: 0, itemsPtr: nil, mutationsPtr: nil,
    extra: (0, 0, 0, 0, 0))
}

/// A dummy value to be used as the target for `mutationsPtr` in fast
/// enumeration implementations.
var _fastEnumerationStorageMutationsTarget: CUnsignedLong = 0

/// A dummy pointer to be used as `mutationsPtr` in fast enumeration
/// implementations.
public // SPI(Foundation)
var _fastEnumerationStorageMutationsPtr: UnsafeMutablePointer<CUnsignedLong> {
  return UnsafeMutablePointer(
      Builtin.addressof(&_fastEnumerationStorageMutationsTarget))
}
#endif
