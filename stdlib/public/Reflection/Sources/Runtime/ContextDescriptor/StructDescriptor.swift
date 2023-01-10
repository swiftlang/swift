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
public struct StructDescriptor: PublicLayout {
  public typealias Layout = (
    base: TypeDescriptor.Layout,
    numberOfFields: UInt32,
    fieldOffsetVectorOffset: UInt32
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension StructDescriptor {
  @inlinable
  public var base: TypeDescriptor {
    TypeDescriptor(ptr)
  }
  
  @inlinable
  public var numberOfFields: Int {
    Int(layout.numberOfFields)
  }
  
  @inlinable
  public var fieldOffsetVectorOffset: Int {
    Int(truncatingIfNeeded: layout.fieldOffsetVectorOffset)
  }
}

extension StructDescriptor {
  @inlinable
  public var genericSignature: GenericSignature? {
    guard base.base.flags.isGeneric else {
      return nil
    }
    
    return getGenericSignature(at: trailing + MemoryLayout<Int32>.size * 2)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension StructDescriptor: Equatable {
  @inlinable
  public static func ==(lhs: StructDescriptor, rhs: StructDescriptor) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension StructDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
