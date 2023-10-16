//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An atomic value.
@available(SwiftStdlib 5.10, *)
@_rawLayout(like: Value.AtomicRepresentation)
@frozen
public struct Atomic<Value: AtomicValue>: ~Copyable {
  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  var address: UnsafeMutablePointer<Value.AtomicRepresentation> {
    UnsafeMutablePointer<Value.AtomicRepresentation>(rawAddress)
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  var rawAddress: Builtin.RawPointer {
    Builtin.unprotectedAddressOfBorrow(self)
  }

  /// Initializes a value of this atomic with the given initial value.
  ///
  /// - Parameter initialValue: The initial value to set this atomic.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public init(_ initialValue: consuming Value) {
    address.initialize(to: Value.encodeAtomicRepresentation(initialValue))
  }

  @inlinable
  deinit {
    address.deinitialize(count: 1)
  }
}

@available(SwiftStdlib 5.10, *)
extension Atomic: @unchecked Sendable where Value: Sendable {}
