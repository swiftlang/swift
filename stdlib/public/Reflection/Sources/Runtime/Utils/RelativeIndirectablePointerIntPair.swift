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

@frozen
public struct RelativeIndirectablePointerIntPair<
  Pointee,
  Integer: FixedWidthInteger
>: RelativePointer {
  public let offset: Int32
  
  @inlinable
  public init(offset: Int32) {
    self.offset = offset
  }
  
  @inline(__always)
  @inlinable
  var intMask: Int {
    (MemoryLayout<Int32>.alignment - 1) & ~1
  }
  
  @inlinable
  public var bool: Bool {
    let offset = Int(truncatingIfNeeded: offset)
    return (offset & intMask) >> 1 != 0
  }
  
  @inlinable
  public var int: Integer {
    let offset = Int(truncatingIfNeeded: offset)
    return Integer(truncatingIfNeeded: (offset & intMask) >> 1)
  }
  
  @inlinable
  public func address(from ptr: UnsafeRawPointer) -> UnsafeRawPointer {
    let unresolved = Int(truncatingIfNeeded: offset) & ~intMask
    
    let address = ptr + (unresolved & ~1)
    
    if unresolved & 1 != 0 {
      return address.loadUnaligned(as: UnsafeRawPointer.self)
    } else {
      return address
    }
  }
}
