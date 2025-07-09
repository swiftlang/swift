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

/// A safe reference allowing in-place reads to a shared value.
///
/// In order to get an instance of an `_Borrow`, one must be able to get shared
/// access to the instance of `Value`. This is achieved through the postfix '&'
/// operator on a shared value.
@available(SwiftStdlib 6.3, *)
@frozen
@safe
public struct _Borrow<Value: ~Copyable>: Copyable, ~Escapable {
  @usableFromInline
  let pointer: UnsafePointer<Value>

  /// Unsafely initializes an instance of `_Borrow` using the given
  /// 'unsafeAddress' as the reference based on the borrowed lifetime of the
  /// given 'owner' argument.
  ///
  /// - Parameter unsafeAddress: The address to use to reference an instance of
  ///                            type `Value`.
  /// - Parameter owner: The owning instance that this `_Borrow` instance's
  ///                    lifetime is based on.
  @available(SwiftStdlib 6.3, *)
  @lifetime(borrow owner)
  @_alwaysEmitIntoClient
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress: UnsafePointer<Value>,
    borrowing owner: borrowing Owner
  ) {
    unsafe pointer = unsafeAddress
  }

  /// Unsafely initializes an instance of `_Borrow` using the given
  /// 'unsafeAddress' as the reference based on the copied lifetime of the given
  /// 'owner' argument.
  ///
  /// - Parameter unsafeAddress: The address to use to reference an instance of
  ///                            type `Value`.
  /// - Parameter owner: The owning instance that this `_Borrow` instance's
  ///                    lifetime is based on.
  @available(SwiftStdlib 6.3, *)
  @lifetime(copy owner)
  @_alwaysEmitIntoClient
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress: UnsafePointer<Value>,
    copying owner: borrowing Owner
  ) {
    unsafe pointer = unsafeAddress
  }

  /// Dereferences the reference allowing for in-place reads to the underlying
  /// instance.
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public subscript() -> Value {
    @_transparent
    unsafeAddress {
      unsafe pointer
    }
  }
}

postfix operator &

/// Returns a reference to 'instance' extending the shared access.
///
/// - Parameter instance: The desired instance to get a reference to.
@available(SwiftStdlib 6.3, *)
@_alwaysEmitIntoClient
@_transparent
public postfix func &<T: ~Copyable>(instance: borrowing @_addressable T) -> _Borrow<T> {
  _Borrow(pointer: UnsafePointer(Builtin.unprotecetedAddressOfBorrow(instance)))
}
