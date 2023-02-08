//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@_alwaysEmitIntoClient
func unsafeBitCast<T, U>(_ x: T, to type: U.Type = U.self) -> U {
  Swift.unsafeBitCast(x, to: type)
}

extension UnsafePointer {
  @_alwaysEmitIntoClient
  var raw: UnsafeRawPointer {
    UnsafeRawPointer(self)
  }
}

extension UnsafeMutablePointer {
  @_alwaysEmitIntoClient
  var raw: UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(self)
  }
}

extension UnsafeBufferPointer {
  @_alwaysEmitIntoClient
  var raw: UnsafeRawBufferPointer {
    UnsafeRawBufferPointer(self)
  }
}

extension UnsafeRawPointer {
  @_alwaysEmitIntoClient
  var bitPattern: UInt64 {
    UInt64(truncatingIfNeeded: UInt(bitPattern: self))
  }
}

extension UInt64 {
  @_alwaysEmitIntoClient
  var rawPointer: UnsafeRawPointer {
    let pointer = UnsafeRawPointer(bitPattern: UInt(truncatingIfNeeded: self))
    
    return pointer.unsafelyUnwrapped
  }
}

extension UnsafeRawPointer {
  @_alwaysEmitIntoClient
  var mutable: UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(mutating: self)
  }
}

extension UnsafeRawPointer {
  @_alwaysEmitIntoClient
  var binaryString: String {
//    let length = strlen(UnsafePointer(_rawValue))
//
//    // This is a hack to make an immortal string.
//    return String(
//      _builtinStringLiteral: _rawValue,
//      utf8CodeUnitCount: length._builtinWordValue,
//      isASCII: Builtin.trunc_Word_Int1(0._builtinWordValue)
//    )
    String(cString: UnsafePointer<CChar>(_rawValue))
  }
}

extension UnsafeRawPointer {
  @_alwaysEmitIntoClient
  func offset(of count: Int) -> UnsafeRawPointer {
    advanced(by: count * MemoryLayout<UnsafeRawPointer>.size)
  }
}

@_alwaysEmitIntoClient
func getSymbolicMangledNameLength(_ base: UnsafeRawPointer) -> Int {
  var end = base
  while let current = Optional(end.load(as: UInt8.self)), current != 0 {
    // Skip the current character
    end = end + 1
    
    // Skip over a symbolic reference
    if current >= 0x1 && current <= 0x17 {
      end += 4
    } else if current >= 0x18 && current <= 0x1F {
      end += MemoryLayout<Int>.size
    }
  }
  
  return end - base
}
