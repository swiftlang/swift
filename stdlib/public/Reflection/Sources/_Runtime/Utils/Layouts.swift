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

@available(SwiftStdlib 9999, *)
public protocol PublicLayout {
  associatedtype Layout
  
  var ptr: UnsafeRawPointer { get }
  
  init(_ ptr: UnsafeRawPointer)
}

@available(SwiftStdlib 9999, *)
extension PublicLayout {
  @inline(__always)
  @inlinable
  public var layout: Layout {
    ptr.loadUnaligned(as: Layout.self)
  }
  
  @inline(__always)
  @inlinable
  public var trailing: UnsafeRawPointer {
    ptr + MemoryLayout<Layout>.size
  }
  
  @inline(__always)
  @inlinable
  public func address<T>(
    for field: KeyPath<Layout, T>
  ) -> UnsafeRawPointer {
    let offset = MemoryLayout<Layout>.offset(of: field).unsafelyUnwrapped
    return ptr + offset
  }
  
  @inline(__always)
  @inlinable
  public func address<T: RelativePointer>(
    for field: KeyPath<Layout, T>
  ) -> UnsafeRawPointer {
    let offset = MemoryLayout<Layout>.offset(of: field).unsafelyUnwrapped
    return layout[keyPath: field].address(from: ptr + offset)
  }
  
  @inline(__always)
  @inlinable
  public func address<T, U>(
    for field: KeyPath<T, U>
  ) -> UnsafeRawPointer where Layout == UnsafePointer<T> {
    let offset = MemoryLayout<T>.offset(of: field).unsafelyUnwrapped
    return UnsafeRawPointer(layout) + offset
  }
}

@available(SwiftStdlib 9999, *)
protocol PrivateLayout {
  associatedtype Layout
  
  var ptr: UnsafeRawPointer { get }
  
  init(_ ptr: UnsafeRawPointer)
}

@available(SwiftStdlib 9999, *)
extension PrivateLayout {
  var layout: Layout {
    ptr.loadUnaligned(as: Layout.self)
  }
  
  var trailing: UnsafeRawPointer {
    ptr + MemoryLayout<Layout>.size
  }
  
  func address<T>(
    for field: KeyPath<Layout, T>
  ) -> UnsafeRawPointer {
    let offset = MemoryLayout<Layout>.offset(of: field)!
    return ptr + offset
  }
  
  func address<T: RelativePointer>(
    for field: KeyPath<Layout, T>
  ) -> UnsafeRawPointer {
    let offset = MemoryLayout<Layout>.offset(of: field)!
    return layout[keyPath: field].address(from: ptr + offset)
  }
}
