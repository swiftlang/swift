//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct UnsafeAtomic<Value> {
  @usableFromInline
  internal let _ptr: UnsafeMutableRawPointer

  @_transparent
  @usableFromInline
  internal init(_raw: UnsafeMutableRawPointer) {
    self._ptr = _raw
  }

  @_transparent // Debug performance
  public init(at address: UnsafeMutablePointer<Value>) {
    self._ptr = UnsafeMutableRawPointer(address)
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomic {
  public static func create(initialValue: __owned Value) -> UnsafeAtomic {
    let ptr = UnsafeMutablePointer<Value>.allocate(capacity: 1)
    ptr.initialize(to: initialValue)
    return UnsafeAtomic(at: ptr)
  }

  __consuming public func destroy() {
    _ptr.deallocate()
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension UnsafeAtomic {
  @inlinable
  public var address: UnsafeMutablePointer<Value> {
    _ptr.assumingMemoryBound(to: Value.self)
  }
}
