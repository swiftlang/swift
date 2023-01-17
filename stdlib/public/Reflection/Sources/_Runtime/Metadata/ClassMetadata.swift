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
public struct ClassMetadata: PublicLayout {
#if canImport(ObjectiveC)
  public typealias Layout = (
    base: Metadata.Layout,
    superclass: Metadata?,
    reserved: (Int, Int),
    rodata: UnsafeRawPointer,
    flags: UInt32,
    instanceAddressPoint: UInt32,
    instanceSize: UInt32,
    instanceAlignmentMask: UInt16,
    runtimeReserved: UInt16,
    classSize: UInt32,
    classAddressPoint: UInt32,
    descriptor: ClassDescriptor,
    ivarDestroyer: UnsafeRawPointer
  )
#else
  public typealias Layout = (
    base: Metadata.Layout,
    superclass: Metadata?,
    flags: UInt32,
    instanceAddressPoint: UInt32,
    instanceSize: UInt32,
    instanceAlignmentMask: UInt16,
    runtimeReserved: UInt16,
    classSize: UInt32,
    classAddressPoint: UInt32,
    descriptor: ClassDescriptor,
    ivarDestroyer: UnsafeRawPointer
  )
#endif
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ClassMetadata {
  @inlinable
  public var superclass: Metadata? {
    guard let superclass = layout.superclass else {
      return nil
    }
    
    return PtrAuth.signSuperclass(superclass)
  }
  
  @inlinable
  public var descriptor: ClassDescriptor {
    PtrAuth.signDescriptor(layout.descriptor)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension ClassMetadata: Equatable {
  @inlinable
  public static func ==(lhs: ClassMetadata, rhs: ClassMetadata) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension ClassMetadata: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
