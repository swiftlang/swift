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
public struct FieldDescriptor: PublicLayout {
  public typealias Layout = (
    mangledTypeName: RelativeDirectPointer<CChar>,
    superclass: RelativeDirectPointer<CChar>,
    kind: UInt16,
    recordSize: UInt16,
    numberOfFields: UInt32
  )
  
  public let ptr: UnsafeRawPointer
  
  @inlinable
  public init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension FieldDescriptor {
  @frozen
  public struct Element: PublicLayout {
    public typealias Layout = (
      flags: Flags,
      mangledTypeName: RelativeDirectPointer<CChar>,
      name: RelativeDirectPointer<CChar>
    )
    
    public let ptr: UnsafeRawPointer
    
    @inlinable
    public init(_ ptr: UnsafeRawPointer) {
      self.ptr = ptr
    }
    
    @inlinable
    public var flags: Flags {
      layout.flags
    }
    
    @inlinable
    @inline(__always)
    public var typeRef: MangledTypeReference {
      MangledTypeReference(address(for: \.mangledTypeName))
    }
    
    @inlinable
    public var name: String {
      address(for: \.name).binaryString
    }
  }
}

extension FieldDescriptor {
  @inlinable
  @inline(__always)
  var fields: IndirectBufferView<Element> {
    IndirectBufferView(
      start: trailing,
      count: Int(truncatingIfNeeded: layout.numberOfFields)
    )
  }
  
  @inlinable
  @inline(__always)
  public subscript(_ position: Int) -> Element {
    fields[position]
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension FieldDescriptor: Equatable {
  @inlinable
  public static func ==(lhs: FieldDescriptor, rhs: FieldDescriptor) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension FieldDescriptor: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
