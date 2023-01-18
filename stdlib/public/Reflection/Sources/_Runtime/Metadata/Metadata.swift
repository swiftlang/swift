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
public struct Metadata: PublicLayout {
  public typealias Layout = Int
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
  
  @inlinable
  public init(_ type: Any.Type) {
    self.ptr = unsafeBitCast(type)
  }
}

@available(SwiftStdlib 5.9, *)
extension Metadata {
  @inlinable
  public var vwt: ValueWitnessTable {
    ValueWitnessTable(ptr.offset(of: -1))
  }
  
  @inlinable
  public var kind: Kind {
    Kind(layout)
  }
}

@available(SwiftStdlib 5.9, *)
extension Metadata {
  @inlinable
  public var `class`: ClassMetadata {
    ClassMetadata(ptr)
  }
  
  @inlinable
  public var `enum`: EnumMetadata {
    EnumMetadata(ptr)
  }
  
  @inlinable
  public var extendedExistential: ExtendedExistentialMetadata {
    ExtendedExistentialMetadata(ptr)
  }
  
  @inlinable
  public var function: FunctionMetadata {
    FunctionMetadata(ptr)
  }
  
  @inlinable
  public var metatype: MetatypeMetadata {
    MetatypeMetadata(ptr)
  }
  
  @inlinable
  public var `struct`: StructMetadata {
    StructMetadata(ptr)
  }
  
  @inlinable
  public var tuple: TupleMetadata {
    TupleMetadata(ptr)
  }
  
  @inlinable
  public var type: TypeMetadata {
    TypeMetadata(ptr)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension Metadata: Equatable {
  @inlinable
  public static func ==(_ lhs: Metadata, _ rhs: Metadata) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

@available(SwiftStdlib 5.9, *)
extension Metadata: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}

@available(SwiftStdlib 5.9, *)
extension Metadata: CustomStringConvertible {
  @inlinable
  public var description: String {
    _typeName(unsafeBitCast(ptr))
  }
}
