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

// TODO: Replace this with Guillaume's actual BufferView
@available(SwiftStdlib 9999, *)
@frozen
public struct BufferView<Element/* : BitwiseCopyable */> {
  public let start: UnsafeRawPointer
  public let count: Int
  
  @inlinable
  var endAddress: UnsafeRawPointer {
    start + MemoryLayout<Element>.size &* count
  }
  
  @inlinable
  init(start: UnsafeRawPointer, count: Int) {
    self.start = start
    self.count = count
  }
}

@available(SwiftStdlib 9999, *)
extension BufferView: RandomAccessCollection {
  @inlinable
  public var startIndex: Int {
    0
  }
  
  @inlinable
  public var endIndex: Int {
    count
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
  public subscript(position: Int) -> Element {
    start.loadUnaligned(
      fromByteOffset: position * MemoryLayout<Element>.size,
      as: Element.self
    )
  }
}

@available(SwiftStdlib 9999, *)
@frozen
public struct IndirectBufferView<Element: PublicLayout> {
  public let start: UnsafeRawPointer
  public let count: Int
  
  @inlinable
  public init(start: UnsafeRawPointer, count: Int) {
    self.start = start
    self.count = count
  }
}

@available(SwiftStdlib 9999, *)
extension IndirectBufferView: RandomAccessCollection {
  @inlinable
  public var startIndex: Int {
    0
  }
  
  @inlinable
  public var endIndex: Int {
    count
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
  @inline(__always)
  public subscript(position: Int) -> Element {
    let address = start.advanced(by: position &* MemoryLayout<Element.Layout>.size)
    return Element(address)
  }
}
