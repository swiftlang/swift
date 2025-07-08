//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.3, *)
@frozen
@safe
public struct _Box<Value: ~Copyable>: ~Copyable {
  @usableFromInline
  let pointer: UnsafeMutablePointer<Value>

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ initialValue: consuming Value) {
    unsafe pointer = UnsafeMutablePointer<Value>.allocate(capacity: 1)
    unsafe pointer.initialize(to: initialValue)
  }

  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    unsafe pointer.deinitialize(count: 1)
    unsafe pointer.deallocate()
  }
}

@available(SwiftStdlib 6.3, *)
extension _Box where Value: ~Copyable {
  @available(SwiftStdlib 6.3, *)
  @lifetime(borrow self)
  @_alwaysEmitIntoClient
  @_transparent
  public func borrow() -> _Borrow<Value> {
    unsafe _Borrow(unsafeAddress: UnsafePointer(pointer), borrowing: self)
  }

  @available(SwiftStdlib 6.3, *)
  @lifetime(&self)
  @_alwaysEmitIntoClient
  @_transparent
  public mutating func mutate() -> _Inout<Value> {
    unsafe _Inout(unsafeAddress: pointer, mutating: &self)
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public var span: Span<Value> {
    @lifetime(borrow self)
    @_transparent
    get {
      unsafe Span(_unsafeStart: pointer, count: 1)
    }
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Value> {
    @lifetime(&self)
    @_transparent
    mutating get {
      unsafe MutableSpan(_unsafeStart: pointer, count: 1)
    }
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public consuming func consume() -> Value {
    let result = unsafe pointer.move()
    unsafe pointer.deallocate()
    discard self
    return result
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public subscript() -> Value {
    @_transparent
    unsafeAddress {
      unsafe UnsafePointer<Value>(pointer)
    }

    @_transparent
    unsafeMutableAddress {
      unsafe pointer
    }
  }
}

@available(SwiftStdlib 6.3, *)
extension _Box where Value: Copyable {
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func copy() -> Value {
    unsafe pointer.pointee
  }
}
