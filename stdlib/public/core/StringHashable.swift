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

import SwiftShims

#if _runtime(_ObjC)
@_silgen_name("swift_stdlib_NSStringHashValue")
func _stdlib_NSStringHashValue(_ str: AnyObject, _ isASCII: Bool) -> Int

@_silgen_name("swift_stdlib_NSStringHashValuePointer")
func _stdlib_NSStringHashValuePointer(_ str: OpaquePointer, _ isASCII: Bool) -> Int
#endif

extension _Unicode {
  internal static func hashASCII(
    _ string: UnsafeBufferPointer<UInt8>
  ) -> Int {
    let collationTable = _swift_stdlib_unicode_getASCIICollationTable()
    var hasher = _SipHash13Context(key: _Hashing.secretKey)
    for c in string {
      _precondition(c <= 127)
      let element = collationTable[Int(c)]
      // Ignore zero valued collation elements. They don't participate in the
      // ordering relation.
      if element != 0 {
        hasher.append(element)
      }
    }
    return hasher._finalizeAndReturnIntHash()
  }

  internal static func hashUTF16(
    _ string: UnsafeBufferPointer<UInt16>
  ) -> Int {
    let collationIterator = _swift_stdlib_unicodeCollationIterator_create(
      string.baseAddress!,
      UInt32(string.count))
    defer { _swift_stdlib_unicodeCollationIterator_delete(collationIterator) }

    var hasher = _SipHash13Context(key: _Hashing.secretKey)
    while true {
      var hitEnd = false
      let element =
        _swift_stdlib_unicodeCollationIterator_next(collationIterator, &hitEnd)
      if hitEnd {
        break
      }
      // Ignore zero valued collation elements. They don't participate in the
      // ordering relation.
      if element != 0 {
        hasher.append(element)
      }
    }
    return hasher._finalizeAndReturnIntHash()
  }
}

extension String : Hashable {
  /// The string's hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  public var hashValue: Int {
#if _runtime(_ObjC)
    // Mix random bits into NSString's hash so that clients don't rely on
    // Swift.String.hashValue and NSString.hash being the same.
#if arch(i386) || arch(arm)
    let hashOffset = Int(bitPattern: 0x88dd_cc21)
#else
    let hashOffset = Int(bitPattern: 0x429b_1266_88dd_cc21)
#endif
    // If we have a contiguous string then we can use the stack optimization.
    let core = self._core
    let isASCII = core.isASCII
    if core.hasContiguousStorage {
      let stackAllocated = _NSContiguousString(core)
      return hashOffset ^ stackAllocated._unsafeWithNotEscapedSelfPointer {
        return _stdlib_NSStringHashValuePointer($0, isASCII)
      }
    } else {
      let cocoaString = unsafeBitCast(
        self._bridgeToObjectiveCImpl(), to: _NSStringCore.self)
      return hashOffset ^ _stdlib_NSStringHashValue(cocoaString, isASCII)
    }
#else
    if let asciiBuffer = self._core.asciiBuffer {
      return _Unicode.hashASCII(UnsafeBufferPointer(
        start: asciiBuffer.baseAddress!,
        count: asciiBuffer.count))
    } else {
      return _Unicode.hashUTF16(
        UnsafeBufferPointer(start: _core.startUTF16, count: _core.count))
    }
#endif
  }
}

