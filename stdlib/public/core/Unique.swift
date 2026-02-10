//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A smart pointer type that uniquely owns an instance of `Value` on the heap.
@available(SwiftStdlib 6.4, *)
@frozen
@safe
public struct Unique<Value: ~Copyable>: ~Copyable {
  @usableFromInline
  let pointer: UnsafeMutablePointer<Value>

  /// Initializes a value of this unqiue box with the given initial value.
  ///
  /// - Parameter initialValue: The initial value to initialize the unqiue box
  ///                           with.
  @available(SwiftStdlib 6.4, *)
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

@available(SwiftStdlib 6.4, *)
extension Unique: Sendable where Value: Sendable & ~Copyable {}

@available(SwiftStdlib 6.4, *)
extension Unique where Value: ~Copyable {
  /// Dereferences the unique box allowing for in-place reads and writes to the
  /// stored `Value`.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public var value: Value {
    @_transparent
    @_unsafeSelfDependentResult
    borrow {
      unsafe pointer.pointee
    }

    @_transparent
    @_unsafeSelfDependentResult
    mutate {
      &(unsafe pointer).pointee
    }
  }

  /// Consumes the unique box and returns the instance of `Value` that was
  /// within the box.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public consuming func consume() -> Value {
    let result = unsafe pointer.move()
    unsafe pointer.deallocate()
    discard self
    return result
  }
}

@available(SwiftStdlib 6.4, *)
extension Unique where Value: ~Copyable {
  /// Returns a single element span reference to the instance of `Value` stored
  /// within this unqiue box.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public var span: Span<Value> {
    @lifetime(borrow self)
    @_transparent
    get {
      unsafe Span(_unsafeStart: pointer, count: 1)
    }
  }

  /// Returns a single element mutable span reference to the instance of `Value`
  /// stored within this unqiue box.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Value> {
    @lifetime(&self)
    @_transparent
    mutating get {
      unsafe MutableSpan(_unsafeStart: pointer, count: 1)
    }
  }
}

@available(SwiftStdlib 6.4, *)
extension Unique where Value: Copyable {
  /// Copies the value within the unqiue box and returns it in a new unique
  /// instance.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public func clone() -> Unique<Value> {
    Unique(value)
  }
}
