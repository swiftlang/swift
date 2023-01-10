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
public struct EnumMetadata: PublicLayout {
  public typealias Layout = (
    base: Metadata.Layout,
    descriptor: EnumDescriptor
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension EnumMetadata {
  @inlinable
  public var vwt: ValueWitnessTable {
    ValueWitnessTable(ptr.offset(of: -1))
  }
  
  @inlinable
  public var enumVWT: EnumValueWitnessTable {
    EnumValueWitnessTable(ptr.offset(of: -1))
  }
  
  @inlinable
  public var descriptor: EnumDescriptor {
    PtrAuth.signDescriptor(layout.descriptor)
  }
}

extension EnumMetadata {
  @inlinable
  public var type: TypeMetadata {
    TypeMetadata(ptr)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension EnumMetadata: Equatable {
  @inlinable
  public static func ==(_ lhs: EnumMetadata, _ rhs: EnumMetadata) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension EnumMetadata: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
