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
import _Runtime

@frozen
public struct Type {
  @usableFromInline
  let metadata: Metadata
  
  @inlinable
  public init(_ type: Any.Type) {
    self.metadata = Metadata(type)
  }
  
  @inlinable
  public init(_ instance: Any) {
    self.metadata = container(for: instance).metadata
  }
  
  @inlinable
  init(_ metadata: Metadata) {
    self.metadata = metadata
  }
}

extension Type {
  @inlinable
  public var isClass: Bool {
    metadata.kind == .class
  }
  
  @inlinable
  public var isEnum: Bool {
    metadata.kind == .enum || metadata.kind == .optional
  }
  
  @inlinable
  public var isExistential: Bool {
    metadata.kind == .existential
  }
  
  @inlinable
  public var isStruct: Bool {
    metadata.kind == .struct
  }
  
  @inlinable
  public var isTuple: Bool {
    metadata.kind == .tuple
  }
}

extension Type {
  @inlinable
  public var swiftType: Any.Type {
    unsafeBitCast(metadata)
  }
}

extension Type {
  @inlinable
  public var genericArguments: GenericArguments {
    guard isClass || isEnum || isStruct else {
      return GenericArguments(nil, 0)
    }
    
    let descriptor = metadata.type.descriptor
    
    guard let genericSignature = descriptor.genericSignature else {
      return GenericArguments(nil, 0)
    }
    
    return GenericArguments(
      metadata.type.genericArguments,
      genericSignature.parameters.count
    )
  }
  
  @inlinable
  public var partial: PartialType? {
    guard isClass || isEnum || isStruct else {
      return nil
    }
    
    return PartialType(metadata.type)
  }
  
  @inlinable
  public var superclass: Type? {
    guard isClass else {
      return nil
    }
    
    return metadata.class.superclass.map { Type($0) }
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension Type: CustomStringConvertible {
  @inlinable
  public var description: String {
    _typeName(unsafeBitCast(metadata.ptr), qualified: false)
  }
}

extension Type: Equatable {
  @inlinable
  public static func ==(_ lhs: Type, _ rhs: Type) -> Bool {
    lhs.metadata == rhs.metadata
  }
}

extension Type: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(metadata)
  }
}
