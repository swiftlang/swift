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

/// A smart pointer type that uniquely owns an instance of `Value` on the heap.
@available(SwiftStdlib 6.3, *)
@frozen
@safe
public struct _Box<Value: ~Copyable>: ~Copyable {
  @usableFromInline
  let pointer: UnsafeMutablePointer<Value>

  /// Initializes a value of this box with the given initial value.
  ///
  /// - Parameter initialValue: The initial value to initialize the box with.
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
  /// Returns a borrowed reference to the instance of `Value` stored within this
  /// box.
  @available(SwiftStdlib 6.3, *)
  @lifetime(borrow self)
  @_alwaysEmitIntoClient
  @_transparent
  public func borrow() -> _Borrow<Value> {
    unsafe _Borrow(unsafeAddress: UnsafePointer(pointer), borrowing: self)
  }

  /// Returns a mutable reference to the instance of `Value` stored within this
  /// box.
  @available(SwiftStdlib 6.3, *)
  @lifetime(&self)
  @_alwaysEmitIntoClient
  @_transparent
  public mutating func mutate() -> _Inout<Value> {
    unsafe _Inout(unsafeAddress: pointer, mutating: &self)
  }

  /// Returns a single element span reference to the instance of `Value` stored
  /// within this box.
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public var span: Span<Value> {
    @lifetime(borrow self)
    @_transparent
    get {
      unsafe Span(_unsafeStart: pointer, count: 1)
    }
  }

  /// Returns a single element mutable span reference to the instance of `Value`
  /// stored within this box.
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Value> {
    @lifetime(&self)
    @_transparent
    mutating get {
      unsafe MutableSpan(_unsafeStart: pointer, count: 1)
    }
  }

  /// Consumes the box, but doesn't deinitialize or deallocate the instance of
  /// `Value` on the heap.
  ///
  /// - Returns: A mutable reference to the leaked value on the heap.
  @available(SwiftStdlib 6.3, *)
  @lifetime(immortal)
  @_alwaysEmitIntoClient
  @_transparent
  public consuming func leak() -> _Inout<Value> {
    let ref = _Inout(unsafeImmortalAddress: pointer)
    discard self
    return ref
  }

  /// Consumes the box and returns the instance of `Value` that was within the
  /// box.
  ///
  /// - Returns: The value that was within the box.
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public consuming func consume() -> Value {
    let result = unsafe pointer.move()
    unsafe pointer.deallocate()
    discard self
    return result
  }

  /// Dereferences the box allowing for in-place reads and writes to the stored
  /// `Value`.
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
  /// Performs a copy of the value within the box.
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func copy() -> Value {
    unsafe pointer.pointee
  }
}
