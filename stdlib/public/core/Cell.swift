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
public struct Cell<Value: ~Copyable>: ~Copyable {
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

@available(SwiftStdlib 6.5, *)
extension Cell where Value: ~Copyable {
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public func wihtUnsafeMutablePointer<Result: ~Copyable, E>(
    _ body: (UnsafeMutablePointer<Value>) throws(E) -> Result
  ) throws(E) -> Result {
    unsafe try body(_address)
  }

  @available(SwiftStdlib 6.5, *)
  @discardableResult
  @export(implementation)
  @_transparent
  public func replace(with newValue: consuming Value) -> Value {
    unsafe exchange(&_address.pointee, with: newValue)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public func set(_ newValue: consuming Value) {
    replace(with: newValue)
  }
}

@available(SwiftStdlib 6.5, *)
extension Cell where Value: Copyable {
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var value: Value {
    get {
      unsafe _address.pointee
    }

    nonmutating set {
      set(newValue)
    }
  }
}
