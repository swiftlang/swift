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
  public var lowercaseString: String {
    if self._core.isASCII {
      let length = self._core.count
      let string = self._core.startASCII
      var buffer = _StringBuffer(
        capacity: length, initialSize: length, elementWidth: 1)
      var pointer = UnsafeMutablePointer<UInt8>(buffer.start)
      for i in 0..<length {
        switch string[i] {
        case let x where (x >= 0x41 && x <= 0x5a):
          pointer[i] = x &+ 0x20
        case let x:
          pointer[i] = x
        }
      }
      return String(_storage: buffer)
    }

    return _ns.lowercaseString
  }

  public var uppercaseString: String {
    if self._core.isASCII {
      let length = self._core.count
      let string = self._core.startASCII
      var buffer = _StringBuffer(
        capacity: length, initialSize: length, elementWidth: 1)
      var pointer = UnsafeMutablePointer<UInt8>(buffer.start)
      for i in 0..<length {
        switch string[i] {
        case let x where (x >= 0x61 && x <= 0x7a):
          pointer[i] = x &- 0x20
        case let x:
          pointer[i] = x
        }
      }
      return String(_storage: buffer)
    }

    return _ns.uppercaseString
  }
}

