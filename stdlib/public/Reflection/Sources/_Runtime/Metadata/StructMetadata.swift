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
public struct StructMetadata: PublicLayout {
  public typealias Layout = (
    base: Metadata.Layout,
    descriptor: StructDescriptor
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 9999, *)
extension StructMetadata {
  @inlinable
  public var descriptor: StructDescriptor {
    PtrAuth.signDescriptor(layout.descriptor)
  }
  
  @inlinable
  public var fieldOffsets: BufferView<UInt32> {
    BufferView(
      start: ptr.offset(of: descriptor.fieldOffsetVectorOffset),
      count: descriptor.numberOfFields
    )
  }
}

@available(SwiftStdlib 9999, *)
extension StructMetadata {
  @inlinable
  public var vwt: ValueWitnessTable {
    ValueWitnessTable(ptr.offset(of: -1))
  }
}

@available(SwiftStdlib 9999, *)
extension StructMetadata {
  @inlinable
  public var type: TypeMetadata {
    TypeMetadata(ptr)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 9999, *)
extension StructMetadata: Equatable {
  @inlinable
  public static func ==(_ lhs: StructMetadata, _ rhs: StructMetadata) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 9999, *)
extension StructMetadata: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
