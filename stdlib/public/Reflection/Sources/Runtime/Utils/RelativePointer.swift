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

public protocol RelativePointer {
  associatedtype Pointee
  
  var offset: Int32 { get }
  
  func address(from ptr: UnsafeRawPointer) -> UnsafeRawPointer
  func pointee(from ptr: UnsafeRawPointer) -> Pointee?
}

extension RelativePointer {
  @inlinable
  public var isNull: Bool {
    offset == 0
  }
  
  @inlinable
  public func pointee(from ptr: UnsafeRawPointer) -> Pointee? {
    if isNull {
      return nil
    }
    
    return address(from: ptr).loadUnaligned(as: Pointee.self)
  }
}
