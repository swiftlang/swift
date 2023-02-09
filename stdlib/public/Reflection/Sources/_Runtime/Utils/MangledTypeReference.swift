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

@available(SwiftStdlib 5.9, *)
@frozen
public struct MangledTypeReference {
  @usableFromInline
  let ptr: UnsafeRawPointer
  
  @inlinable
  init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension MangledTypeReference {
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public var length: Int {
    getSymbolicMangledNameLength(ptr)
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  var standardSubstitution: Any.Type? {
    let byte1 = ptr.unprotectedLoad(fromByteOffset: 1, as: UInt8.self)
    
    guard byte1 != 0 else {
      return nil
    }
    
    let byte2 = ptr.unprotectedLoad(fromByteOffset: 2, as: UInt8.self)
    
    guard byte2 == 0 else {
      return nil
    }
    
    let byte0 = ptr.unprotectedLoad(as: UInt8.self)
    
    switch (byte0, byte1) {
    // Stdlib types
    case (UInt8(ascii: "S"), _):
      switch byte1 {
      // Bool
      case UInt8(ascii: "b"):
        return Bool.self
      
      // Double
      case UInt8(ascii: "d"):
        return Double.self
        
      // Float
      case UInt8(ascii: "f"):
        return Float.self
        
      // Int
      case UInt8(ascii: "i"):
        return Int.self
        
      // Character
      case UInt8(ascii: "J"):
        return Character.self
        
      // ObjectIdentifier
      case UInt8(ascii: "O"):
        return ObjectIdentifier.self
        
      // String
      case UInt8(ascii: "S"):
        return String.self
        
      // Substring
      case UInt8(ascii: "s"):
        return Substring.self
        
      // UInt
      case UInt8(ascii: "u"):
        return UInt.self

      // UnsafeRawPointer
      case UInt8(ascii: "V"):
        return UnsafeRawPointer.self

      // UnsafeMutableRawPointer
      case UInt8(ascii: "v"):
        return UnsafeMutableRawPointer.self

      // UnsafeRawBufferPointer
      case UInt8(ascii: "W"):
        return UnsafeRawBufferPointer.self

      // UnsafeMutableRawBufferPointer
      case UInt8(ascii: "w"):
        return UnsafeMutableRawBufferPointer.self
        
      default:
        return nil
      }
    
    // Any
    case (UInt8(ascii: "y"), UInt8(ascii: "p")):
      return Any.self
    
    default:
      return nil
    }
  }
}
