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
public struct RelativeIndirectPointer<Pointee>: RelativePointer {
  public let offset: Int32
  
  @inlinable
  public init(offset: Int32) {
    self.offset = offset
  }
  
  @inlinable
  public func address(from ptr: UnsafeRawPointer) -> UnsafeRawPointer {
    (ptr + Int(offset)).unprotectedLoad(as: UnsafeRawPointer.self)
  }
}

extension UnsafeRawPointer {
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public func relativeIndirectAddress<T>(as type: T.Type) -> UnsafeRawPointer {
    let relativePointer = RelativeIndirectPointer<T>(
      offset: unprotectedLoad(as: Int32.self)
    )
    
    return relativePointer.address(from: self)
  }
}
