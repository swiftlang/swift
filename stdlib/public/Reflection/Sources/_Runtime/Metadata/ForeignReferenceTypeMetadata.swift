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
public struct ForeignReferenceTypeMetadata: PublicLayout {
  public typealias Layout = (
    base: Metadata.Layout,
    descriptor: ClassDescriptor
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension ForeignReferenceTypeMetadata {
  @inlinable
  public var descriptor: ClassDescriptor {
    PtrAuth.signDescriptor(layout.descriptor)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension ForeignReferenceTypeMetadata: Equatable {
  @inlinable
  public static func ==(
    lhs: ForeignReferenceTypeMetadata,
    rhs: ForeignReferenceTypeMetadata
  ) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension ForeignReferenceTypeMetadata: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
