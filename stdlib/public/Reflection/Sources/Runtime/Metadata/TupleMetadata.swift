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
public struct TupleMetadata: PrivateLayout {
  typealias Layout = (
    base: Metadata.Layout,
    numberOfElements: Int,
    labels: UnsafePointer<CChar>?
  )
  
  let ptr: UnsafeRawPointer
  
  @usableFromInline
  init(_ ptr: UnsafeRawPointer) {
    self.ptr = ptr
  }
}

extension TupleMetadata {
  @frozen
  public struct Elements {
    @usableFromInline
    let metadata: TupleMetadata
    
    @inlinable
    init(_ metadata: TupleMetadata) {
      self.metadata = metadata
    }
  }
}

extension TupleMetadata.Elements {
  @frozen
  public struct Element: PrivateLayout {
#if canImport(Darwin)
    typealias Layout = (
      metadata: Metadata,
      offset: Int
    )
#else
    typealias Layout = (
      metadata: Metadata,
      offset: UInt32,
      padding: UInt32
    )
#endif
    
    let ptr: UnsafeRawPointer
    
    let index: Int
    
    init(_ ptr: UnsafeRawPointer) {
      self.ptr = ptr
      self.index = 0
    }
    
    init(_ ptr: UnsafeRawPointer, _ index: Int) {
      self.ptr = ptr
      self.index = index
    }
  }
}

extension TupleMetadata.Elements.Element {
  public var label: String {
    // Go back up n times the size of our layout to get the beginning element
    // pointer
    var address = ptr - MemoryLayout<Layout>.size &* index
    // Finally, go back another word to get a pointer to a pointer of the label
    // string
    address -= MemoryLayout<Int>.size
    
    guard let cString = address.loadUnaligned(
      as: UnsafePointer<CChar>?.self
    ) else {
      return ""
    }
    
    let labels = String(cString: cString)
    
    var previousSpaceIndex = labels.utf8.startIndex
    var spaceIndex = labels.utf8.startIndex
    
    for _ in 0 ..< index + 1 {
      if index != 0 {
        previousSpaceIndex = labels.utf8.index(after: spaceIndex)
      }
      
      spaceIndex = labels.utf8[previousSpaceIndex...].firstIndex(
        of: UInt8(ascii: " ")
      ).unsafelyUnwrapped
    }
    
    return String(labels[previousSpaceIndex ..< spaceIndex])
  }
  
  public var metadata: Metadata {
    layout.metadata
  }
  
  public var offset: Int {
    Int(truncatingIfNeeded: layout.offset)
  }
}

extension TupleMetadata.Elements: RandomAccessCollection {
  @inlinable
  public var startIndex: Int {
    0
  }
  
  public var endIndex: Int {
    metadata.layout.numberOfElements
  }
  
  @inlinable
  public func index(after i: Int) -> Int {
    i + 1
  }
  
  @inlinable
  public func index(before i: Int) -> Int {
    i - 1
  }
  
  public subscript(_ position: Int) -> Element {
    let ptr = metadata.trailing + MemoryLayout<Element.Layout>.size &* position
    return Element(ptr, position)
  }
}

extension TupleMetadata {
  @inlinable
  public var elements: Elements {
    Elements(self)
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

extension TupleMetadata: Equatable {
  public static func ==(lhs: TupleMetadata, rhs: TupleMetadata) -> Bool {
    lhs.ptr == rhs.ptr
  }
}

extension TupleMetadata: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}
