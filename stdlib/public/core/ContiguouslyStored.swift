//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension String {

  @inlinable @inline(__always)
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    if _fastPath(self._guts.isFastUTF8) {
      return try self._guts.withFastUTF8 {
        try body($0)
      }
    }

    return try ContiguousArray(self.utf8).withUnsafeBufferPointer {
      try body($0)
    }
  }
}
extension Substring {
  
  @inlinable @inline(__always)
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    if _fastPath(_wholeGuts.isFastUTF8) {
      return try _wholeGuts.withFastUTF8(range: self._offsetRange) {
        return try body($0)
      }
    }

    return try ContiguousArray(self.utf8).withUnsafeBufferPointer {
      try body($0)
    }
  }
}
