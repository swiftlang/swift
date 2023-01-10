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
public struct OpaqueDescriptor: PublicLayout {
  public typealias Layout = ContextDescriptor.Layout
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension OpaqueDescriptor {
  @inlinable
  public var numberOfUnderlyingTypes: Int {
    Int(truncatingIfNeeded: layout.flags.kindSpecificFlags)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension OpaqueDescriptor: Equatable {
  @inlinable
  public static func ==(lhs: OpaqueDescriptor, rhs: OpaqueDescriptor) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension OpaqueDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
