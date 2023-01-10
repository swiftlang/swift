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
public struct ForeignClassMetadata: PublicLayout {
  public typealias Layout = (
    base: Metadata.Layout,
    descriptor: ClassDescriptor,
    superclass: Metadata?
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 9999, *)
extension ForeignClassMetadata {
  @inlinable
  public var descriptor: ClassDescriptor {
    PtrAuth.signDescriptor(layout.descriptor)
  }
  
  @inlinable
  public var superclass: Metadata? {
    layout.superclass
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 9999, *)
extension ForeignClassMetadata: Equatable {
  @inlinable
  public static func ==(
    lhs: ForeignClassMetadata,
    rhs: ForeignClassMetadata
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 9999, *)
extension ForeignClassMetadata: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
