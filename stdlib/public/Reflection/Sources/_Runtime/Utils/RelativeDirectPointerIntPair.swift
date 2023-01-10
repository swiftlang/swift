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

@available(SwiftStdlib 9999, *)
@frozen
public struct RelativeDirectPointerIntPair<
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
    MemoryLayout<Int32>.alignment - 1
  }
  
  @inlinable
  public var bool: Bool {
    let offset = Int(truncatingIfNeeded: offset)
    return offset & intMask != 0
  }
  
  @inlinable
  public var int: Integer {
    let offset = Int(truncatingIfNeeded: offset)
    return Integer(truncatingIfNeeded: offset & intMask)
  }
  
  @inlinable
  public func address(from ptr: UnsafeRawPointer) -> UnsafeRawPointer {
    ptr + (Int(truncatingIfNeeded: offset) & ~intMask)
  }
}
