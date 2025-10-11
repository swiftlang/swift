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
@unsafe
@_rawLayout(like: Value, movesAsLike)
public struct _UnsafeCell<Value: ~Copyable>: ~Copyable {
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var address: UnsafeMutablePointer<Value> {
    unsafe UnsafeMutablePointer<Value>(rawAddress)
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var rawAddress: Builtin.RawPointer {
    unsafe Builtin.addressOfRawLayout(self)
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ initialValue: consuming Value) {
    unsafe address.initialize(to: initialValue)
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    unsafe address.deinitialize(count: 1)
  }
}

@available(SwiftStdlib 6.3, *)
@frozen
@safe
public struct _Cell<Value: ~Copyable>: ~Copyable {
  @usableFromInline
  let cell: _UnsafeCell<Value>

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ initialValue: consuming Value) {
    unsafe cell = _UnsafeCell(initialValue)
  }
}

@available(SwiftStdlib 6.3, *)
extension _Cell where Value: ~Copyable {
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var address: UnsafeMutablePointer<Value> {
    unsafe cell.address
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func replace(with new: consuming Value) -> Value {
    let old = unsafe address.move()
    unsafe address.initialize(to: new)
    return old
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func set(_ new: consuming Value) {
    _ = replace(with: new)
  }
}

@available(SwiftStdlib 6.3, *)
extension _Cell where Value: Copyable {
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func get() -> Value {
    unsafe address.pointee
  }
}
