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
public struct RelativeIndirectablePointer<Pointee>: RelativePointer {
  public let offset: Int32
  
  @inlinable
  public init(offset: Int32) {
    self.offset = offset
  }
  
  @inlinable
  public func address(from ptr: UnsafeRawPointer) -> UnsafeRawPointer {
    let address = ptr + Int(truncatingIfNeeded: offset & ~1)
    
    if offset & 1 != 0 {
      return address.loadUnaligned(as: UnsafeRawPointer.self)
    } else {
      return address
    }
  }
}
