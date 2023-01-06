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
import Runtime

@frozen
public struct PartialType {
  @usableFromInline
  let descriptor: TypeDescriptor
  
  @inlinable
  init(_ metadata: TypeMetadata) {
    self.descriptor = metadata.descriptor
  }
}

extension PartialType {
  @inlinable
  public var isGeneric: Bool {
    descriptor.base.flags.isGeneric
  }
  
  @inlinable
  public var name: String {
    descriptor.name
  }
}

extension PartialType {
  @inlinable
  public func create() -> Type? {
    guard !descriptor.base.flags.isGeneric else {
      return nil
    }
    
    return Type(descriptor.accessor(.complete))
  }
  
  @inlinable
  public func create(with arg: Type) -> Type? {
    guard let genericSig = descriptor.genericSignature,
          genericSig.parameters.count == 1 else {
      return nil
    }
    
    return Type(descriptor.accessor(.complete, arg.metadata))
  }
  
  @inlinable
  public func create(with arg0: Type, _ arg1: Type) -> Type? {
    guard let genericSig = descriptor.genericSignature,
          genericSig.parameters.count == 2 else {
      return nil
    }
    
    return Type(descriptor.accessor(.complete, arg0.metadata, arg1.metadata))
  }
  
  @inlinable
  public func create(with arg0: Type, _ arg1: Type, _ arg2: Type) -> Type? {
    guard let genericSig = descriptor.genericSignature,
          genericSig.parameters.count == 3 else {
      return nil
    }
    
    return Type(descriptor.accessor(
      .complete,
      arg0.metadata,
      arg1.metadata,
      arg2.metadata
    ))
  }
  
  @inlinable
  public func create(with args: Type...) -> Type? {
    guard let genericSig = descriptor.genericSignature,
          genericSig.parameters.count == args.count else {
      return nil
    }
    
    let metadataArgs = args.map {
      $0.metadata
    }
    
    return Type(descriptor.accessor(.complete, metadataArgs))
  }
}

extension PartialType {
  @inlinable
  public func create(with arg: Any.Type) -> Type? {
    guard let genericSig = descriptor.genericSignature,
          genericSig.parameters.count == 1 else {
      return nil
    }
    
    return Type(descriptor.accessor(.complete, Metadata(arg)))
  }
  
  @inlinable
  public func create(with arg0: Any.Type, _ arg1: Any.Type) -> Type? {
    guard let genericSig = descriptor.genericSignature,
          genericSig.parameters.count == 2 else {
      return nil
    }
    
    return Type(descriptor.accessor(.complete, Metadata(arg0), Metadata(arg1)))
  }
  
  @inlinable
  public func create(
    with arg0: Any.Type,
    _ arg1: Any.Type,
    _ arg2: Any.Type
  ) -> Type? {
    guard let genericSig = descriptor.genericSignature,
          genericSig.parameters.count == 3 else {
      return nil
    }
    
    return Type(descriptor.accessor(
      .complete,
      Metadata(arg0),
      Metadata(arg1),
      Metadata(arg2)
    ))
  }
  
  @inlinable
  public func create(with args: Any.Type...) -> Type? {
    guard let genericSig = descriptor.genericSignature,
          genericSig.parameters.count == args.count else {
      return nil
    }
    
    let metadataArgs = args.map {
      Metadata($0)
    }
    
    return Type(descriptor.accessor(.complete, metadataArgs))
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension PartialType: Equatable {
  @inlinable
  public static func ==(_ lhs: PartialType, _ rhs: PartialType) -> Bool {
    lhs.descriptor == rhs.descriptor
  }
}

extension PartialType: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(descriptor)
  }
}
