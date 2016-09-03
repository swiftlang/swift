//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

#if _runtime(_ObjC)
@_silgen_name("swift_stdlib_NSStringHashValue")
func _stdlib_NSStringHashValue(_ str: AnyObject, _ isASCII: Bool) -> Int

@_silgen_name("swift_stdlib_NSStringHashValuePointer")
func _stdlib_NSStringHashValuePointer(_ str: OpaquePointer, _ isASCII: Bool) -> Int
#endif

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
    if self._core.isASCII {
      return _swift_stdlib_unicode_hash_ascii(
        _core.startASCII, Int32(_core.count))
    } else {
      return _swift_stdlib_unicode_hash(_core.startUTF16, Int32(_core.count))
    }
#endif
  }
}

