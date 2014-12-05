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

// FIXME: these properties should be implemented in the core library.
// <rdar://problem/17550602> [unicode] Implement case folding
extension String {
  /// A "table" for which ASCII characters need to be upper cased.
  /// To determine which bit corresponds to which ASCII character, subtract 1
  /// from the ASCII value of that character and divide by 2. The bit is set iff
  /// that character is a lower case character.
  internal var _asciiLowerCaseTable: UInt64 {
    @inline(__always)
    get {
      return 0b0001_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    }
  }

  /// The same table for upper case characters.
  internal var _asciiUpperCaseTable: UInt64 {
    @inline(__always)
    get {
      return 0b0000_0000_0000_0000_0001_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000
    }
  }

  public var lowercaseString: String {
    if self._core.isASCII {
      let length = self._core.count
      let source = self._core.startASCII
      var buffer = _StringBuffer(
        capacity: length, initialSize: length, elementWidth: 1)
      var dest = UnsafeMutablePointer<UInt8>(buffer.start)
      for i in 0..<length {
        // For each character in the string, we lookup if it should be shifted
        // in our ascii table, then we return 0x20 if it should, 0x0 if not.
        // This code is equivalent to:
        // switch source[i] {
        // case let x where (x >= 0x41 && x <= 0x5a):
        //   dest[i] = x &+ 0x20
        // case let x:
        //   dest[i] = x
        // }
        let value = source[i]
        let isUpper =
          _asciiUpperCaseTable >>
          UInt64(((value &- 1) & 0b0111_1111) >> 1)
        let add = (isUpper & 0x1) << 5
        dest[i] = value + UInt8(add)
      }
      return String(_storage: buffer)
    }

    return _ns.lowercaseString
  }

  public var uppercaseString: String {
    if self._core.isASCII {
      let length = self._core.count
      let source = self._core.startASCII
      var buffer = _StringBuffer(
        capacity: length, initialSize: length, elementWidth: 1)
      var dest = UnsafeMutablePointer<UInt8>(buffer.start)
      for i in 0..<length {
        // See the comment above in lowercaseString.
        let value = source[i]
        let isLower =
          _asciiLowerCaseTable >>
          UInt64(((value &- 1) & 0b0111_1111) >> 1)
        let add = (isLower & 0x1) << 5
        dest[i] = value - UInt8(add)
      }
      return String(_storage: buffer)
    }

    return _ns.uppercaseString
  }
}
