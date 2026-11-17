//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.0, *)
@backDeployed(before: SwiftStdlib 6.5)
@frozen
@_rawLayout(like: Value, movesAsLike)
public struct ConstCell<Value: ~Copyable>: ~Copyable {
  @export(implementation)
  @_transparent
  internal var _address: UnsafeMutablePointer<Value> {
    unsafe UnsafeMutablePointer<Value>(_rawAddress)
  }

  @export(implementation)
  @_transparent
  internal var _rawAddress: Builtin.RawPointer {
    Builtin.addressOfRawLayout(self)
  }

  @export(implementation)
  @_transparent
  public init(_ initialValue: consuming Value) {
    unsafe _address.initialize(to: initialValue)
  }

  @export(implementation)
  deinit {
    unsafe _address.deinitialize(count: 1)
  }
}

extension ConstCell where Value: ~Copyable {
  @export(implementation)
  @_transparent
  public func withUnsafePointer<Result: ~Copyable, E>(
    _ body: (UnsafePointer<Value>) throws(E) -> Result
  ) throws(E) -> Result {
    unsafe try body(UnsafePointer(_address))
  }
}

extension ConstCell where Value: Copyable {
  @export(implementation)
  @_transparent
  public var value: Value {
    unsafe _address.pointee
  }
}
