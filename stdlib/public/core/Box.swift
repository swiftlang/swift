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
  @_usableFromInline
  let _pointer: UnsafeMutablePointer<Value>

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ initialValue: consuming Value) {
    _pointer = UnsafeMutablePointer<Value>.allocate(capacity: 1)
    _pointer.initialize(to: initialValue)
  }

  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    _pointer.deinitialize(count: 1)
    _pointer.deallocate()
  }
}

@available(SwiftStdlib 6.3, *)
extension Box where Value: ~Copyable {
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public var span: Span<Value> {
    @lifetime(borrow self)
    @_transparent
    get {
      unsafe Span(_unsafeStart: _pointer, count: 1)
    }
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Value> {
    @lifetime(&self)
    @_transparent
    mutating get {
      unsafe MutableSpan(_unsafeStart: _pointer, count: 1)
    }
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func consume() -> Value {
    let result = _pointer.move()
    _pointer.deallocate()
    discard self
    return result
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public subscript() -> Value {
    @_transparent
    unsafeAddress {
      UnsafePointer<Value>(_pointer)
    }

    @_transparent
    unsafeMutableAddress {
      _pointer
    }
  }
}

@available(SwiftStdlib 6.3, *)
extension Box where Value: Copyable {
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func copy() -> Value {
    _pointer.pointee
  }
}
