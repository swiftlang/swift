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

@available(SwiftStdlib 9999, *)
@frozen
public struct Field {
  @usableFromInline
  let index: Int
  
  @usableFromInline
  let parent: Metadata
  
  @inlinable
  init(index: Int, parent: Metadata) {
    self.index = index
    self.parent = parent
  }
}

@available(SwiftStdlib 9999, *)
extension Field {
  @inlinable
  public var isVar: Bool {
    guard parent.kind == .struct || parent.kind == .class else {
      return false
    }
    
    return parent.type.descriptor.fields[index].flags.isVar
  }
  
  @inlinable
  public var name: String {
    guard parent.kind != .tuple else {
      //return TupleMetadata(parent.ptr).elements[index].
      return "hello"
    }
    
    return parent.type.descriptor.fields[index].name
  }
  
  @inlinable
  public var offset: Int {
    switch parent.kind {
    case .struct:
      return Int(truncatingIfNeeded: parent.struct.fieldOffsets[index])
    
    case .tuple:
      return parent.tuple.elements[index].offset
      
    default:
      return 0
    }
  }
  
  @inlinable
  public var type: Type {
    guard parent.kind != .tuple else {
      return Type(parent.tuple.elements[index].metadata)
    }
    
    let typeRef = parent.type.descriptor.fields[index].typeRef
    
    return Type(parent.type.resolve(typeRef).unsafelyUnwrapped)
  }
  
  @inlinable
  public var keyPath: AnyKeyPath {
    func openedRoot<Root>(_: Root.Type) -> AnyKeyPath {
      func openedValue<Value>(_: Value.Type) -> AnyKeyPath {
        KeyPath<Root, Value>.create(for: self)
      }
      
      return _openExistential(
        type.swiftType,
        do: openedValue(_:)
      )
    }
    
    return _openExistential(
      Type(parent).swiftType,
      do: openedRoot(_:)
    )
  }
}

@available(SwiftStdlib 9999, *)
extension Field: CustomStringConvertible {
  @inlinable
  public var description: String {
    var result = ""
    
    if isVar {
      result = "var "
    } else {
      result = "let "
    }
    
    result += "\(name): \(type)"
    
    return result
  }
}

@available(SwiftStdlib 9999, *)
@frozen
public struct Fields {
  @usableFromInline
  let metadata: Metadata
  
  @inlinable
  init(_ metadata: Metadata) {
    self.metadata = metadata
  }
}

@available(SwiftStdlib 9999, *)
extension Fields: RandomAccessCollection {
  @inlinable
  public var startIndex: Int {
    0
  }
  
  @inlinable
  public var endIndex: Int {
    switch metadata.kind {
    case .class:
      return metadata.class.descriptor.numberOfFields
    
    case .struct:
      return metadata.struct.descriptor.numberOfFields
      
    case .tuple:
      return metadata.tuple.elements.count
      
    default:
      return 0
    }
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
  public subscript(_ position: Int) -> Field {
    Field(index: position, parent: metadata)
  }
}

@available(SwiftStdlib 9999, *)
extension Type {
  @inlinable
  public var fields: Fields {
    Fields(metadata)
  }
}
