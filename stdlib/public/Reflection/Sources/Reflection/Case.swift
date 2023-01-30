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

@available(SwiftStdlib 5.9, *)
@frozen
public struct Case {
  @usableFromInline
  let parent: EnumMetadata
  
  @usableFromInline
  let tag: Int
  
  @inlinable
  init(parent: EnumMetadata, tag: Int) {
    self.parent = parent
    self.tag = tag
  }
  
  @inlinable
  public init?(from instance: Any) {
    guard Type(instance).isEnum else {
      return nil
    }
    
    var container = unsafeBitCast(instance, to: AnyExistentialContainer.self)
    
    let tag = container.projectValue {
      Metadata(type(of: instance)).enum.enumVWT.getEnumTag($0)
    }
    
    self.parent = Metadata(type(of: instance)).enum
    self.tag = Int(truncatingIfNeeded: tag)
  }
}

@available(SwiftStdlib 5.9, *)
extension Case {
  @inlinable
  public var hasPayload: Bool {
    tag < parent.descriptor.numberOfPayloadCases
  }
  
  @inlinable
  public var isIndirect: Bool {
    parent.descriptor.type.fields[tag].flags.isIndirectCase
  }
  
  @inlinable
  public var name: String {
    parent.descriptor.type.fields[tag].name
  }
  
  @inlinable
  public var payloadType: Type? {
    guard tag < parent.descriptor.numberOfPayloadCases else {
      return nil
    }
    
    let typeRef = parent.descriptor.type.fields[tag].typeRef
    
    guard let resolved = parent.type.resolve(typeRef) else {
      return nil
    }
    
    return Type(resolved)
  }
}

@available(SwiftStdlib 5.9, *)
extension Case: CustomStringConvertible {
  @inlinable
  public var description: String {
    var result = "\(unsafeBitCast(parent, to: Any.Type.self)).\(name)"
    
    if hasPayload {
      let ty = payloadType!
      
      if ty.isTuple {
        result += "\(ty)"
      } else {
        result += "(\(ty))"
      }
    }
    
    return result
  }
}

@available(SwiftStdlib 5.9, *)
@frozen
public struct Cases {
  @usableFromInline
  let metadata: Metadata
  
  @inlinable
  init(_ metadata: Metadata) {
    self.metadata = metadata
  }
}

@available(SwiftStdlib 5.9, *)
extension Cases: RandomAccessCollection {
  @inlinable
  public var startIndex: Int {
    0
  }
  
  @inlinable
  public var endIndex: Int {
    guard Type(metadata).isEnum else {
      return 0
    }
    
    return metadata.enum.descriptor.numberOfCases
  }
  
  @inlinable
  public func index(after i: Int) -> Int {
    i + 1
  }
  
  @inlinable
  public func index(before i: Int) -> Int {
    i - 1
  }
  
  @inlinable
  public subscript(_ position: Int) -> Case {
    precondition(position < endIndex)
    
    return Case(parent: metadata.enum, tag: position)
  }
}

@available(SwiftStdlib 5.9, *)
extension Type {
  @inlinable
  public var cases: Cases {
    Cases(metadata)
  }
}
