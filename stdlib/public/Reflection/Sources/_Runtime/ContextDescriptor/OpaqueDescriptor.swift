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
public struct OpaqueDescriptor: PublicLayout {
  public typealias Layout = ContextDescriptor.Layout
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 9999, *)
extension OpaqueDescriptor {
  @inlinable
  public var numberOfUnderlyingTypes: Int {
    Int(truncatingIfNeeded: layout.flags.kindSpecificFlags)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 9999, *)
extension OpaqueDescriptor: Equatable {
  @inlinable
  public static func ==(lhs: OpaqueDescriptor, rhs: OpaqueDescriptor) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 9999, *)
extension OpaqueDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
