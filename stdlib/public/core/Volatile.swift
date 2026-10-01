//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.5, *)
@frozen
@_rawLayout(like: Value, movesAsLike)
public struct Volatile<Value>: ~Copyable {
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  internal var _address: UnsafeMutablePointer<Value> {
    unsafe UnsafeMutablePointer<Value>(_rawAddress)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  internal var _rawAddress: Builtin.RawPointer {
    Builtin.addressOfRawLayout(self)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public init(_ initialValue: consuming Value) {
    unsafe _address.initialize(to: initialValue)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  deinit {
    unsafe _address.deinitialize(count: 1)
  }
}

// UInt8

@available(SwiftStdlib 6.5, *)
extension Volatile where Value == UInt8 {
@available(SwiftStdlib 6.5, *)
@export(implementation)
@_transparent
  public func read() -> UInt8 {
    UInt8(Builtin.atomicload_monotonic_volatile_Int8(_rawAddress))
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func write(_ newValue: UInt8) {
    Builtin.atomicstore_monotonic_volatile_Int8(_rawAddress, newValue._value)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func modify<Result: ~Copyable, E>(
    _ body: (inout UInt8) throws(E) -> Result
  ) throws(E) -> Result {
    var r = read()
    let result = try body(&r)
    write(r)
    return result
  }
}

// UInt16

@available(SwiftStdlib 6.5, *)
extension Volatile where Value == UInt16 {
@available(SwiftStdlib 6.5, *)
@export(implementation)
@_transparent
  public func read() -> UInt16 {
    UInt16(Builtin.atomicload_monotonic_volatile_Int16(_rawAddress))
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func write(_ newValue: UInt16) {
    Builtin.atomicstore_monotonic_volatile_Int16(_rawAddress, newValue._value)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func modify<Result: ~Copyable, E>(
    _ body: (inout UInt16) throws(E) -> Result
  ) throws(E) -> Result {
    var r = read()
    let result = try body(&r)
    write(r)
    return result
  }
}

// UInt32

@available(SwiftStdlib 6.5, *)
extension Volatile where Value == UInt32 {
@available(SwiftStdlib 6.5, *)
@export(implementation)
@_transparent
  public func read() -> UInt32 {
    UInt32(Builtin.atomicload_monotonic_volatile_Int32(_rawAddress))
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func write(_ newValue: UInt32) {
    Builtin.atomicstore_monotonic_volatile_Int32(_rawAddress, newValue._value)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func modify<Result: ~Copyable, E>(
    _ body: (inout UInt32) throws(E) -> Result
  ) throws(E) -> Result {
    var r = read()
    let result = try body(&r)
    write(r)
    return result
  }
}

// UInt64

@available(SwiftStdlib 6.5, *)
extension Volatile where Value == UInt64 {
@available(SwiftStdlib 6.5, *)
@export(implementation)
@_transparent
  public func read() -> UInt64 {
    UInt64(Builtin.atomicload_monotonic_volatile_Int64(_rawAddress))
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func write(_ newValue: UInt64) {
    Builtin.atomicstore_monotonic_volatile_Int64(_rawAddress, newValue._value)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func modify<Result: ~Copyable, E>(
    _ body: (inout UInt64) throws(E) -> Result
  ) throws(E) -> Result {
    var r = read()
    let result = try body(&r)
    write(r)
    return result
  }
}
