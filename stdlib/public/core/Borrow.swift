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
public struct _Borrow<Value: ~Copyable>: Copyable, ~Escapable {
  @usableFromInline
  let _pointer: UnsafePointer<Value>

  @available(SwiftStdlib 6.3, *)
  @lifetime(borrow value)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ value: borrowing @_addressable Value) {
    unsafe _pointer = UnsafePointer(Builtin.unprotectedAddressOfBorrow(value))
  }

  @available(SwiftStdlib 6.3, *)
  @lifetime(borrow owner)
  @_alwaysEmitIntoClient
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress: UnsafePointer<Value>,
    borrowing owner: borrowing Owner
  ) {
    unsafe _pointer = unsafeAddress
  }

  @available(SwiftStdlib 6.3, *)
  @lifetime(copy owner)
  @_alwaysEmitIntoClient
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress: UnsafePointer<Value>,
    copying owner: borrowing Owner
  ) {
    unsafe _pointer = unsafeAddress
  }

  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public subscript() -> T {
    @_transparent
    unsafeAddress {
      unsafe _pointer
    }
  }
}