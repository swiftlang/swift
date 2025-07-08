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

/// A safe mutable reference allowing in-place mutation to an exclusive value.
///
/// In order to get an instance of an `Inout<Value>`, one must have exclusive access
/// to the instance of `Value`. This is achieved through the 'inout' operator, '&'.
@available(SwiftStdlib 6.3, *)
@frozen
@safe
public struct Inout<Value: ~Copyable>: ~Copyable, ~Escapable {
  @usableFromInline
  let pointer: UnsafeMutablePointer<Value>

  /// Initializes an instance of 'Inout' extending the exclusive access of the
  /// passed instance.
  ///
  /// - Parameter instance: The desired instance to get a mutable reference to.
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init(_ instance: inout Value) {
    unsafe pointer = UnsafeMutablePointer(Builtin.unprotectedAddressOf(&instance))
  }

  /// Unsafely initializes an instance of 'Inout' using the given 'unsafeAddress'
  /// as the mutable reference based on the lifetime of the given 'owner'
  /// argument.
  ///
  /// - Parameter unsafeAddress: The address to use to mutably reference an
  ///                            instance of type 'Value'.
  /// - Parameter owner: The owning instance that this 'Inout' instance's
  ///                    lifetime is based on.
  @available(SwiftStdlib 6.3, *)
  @lifetime(&owner)
  @unsafe
  @_alwaysEmitIntoClient
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress: UnsafeMutablePointer<Value>,
    mutating owner: inout Owner
  ) {
    unsafe pointer = unsafeAddress
  }

  /// Unsafely initializes an instance of 'Inout' using the given
  /// 'unsafeImmortalAddress' as the mutable reference acting as though its
  /// lifetime is immortal.
  ///
  /// - Parameter unsafeImmortalAddress: The address to use to mutably reference
  ///                                    an immortal instance of type 'Value'.
  @available(SwiftStdlib 6.3, *)
  @lifetime(immortal)
  @unsafe
  @_alwaysEmitIntoClient
  @_transparent
  public init(
    unsafeImmortalAddress: UnsafeMutablePointer<Value>
  ) {
    unsafe pointer = unsafeImmortalAddress
  }
}

@available(SwiftStdlib 6.3, *)
extension Inout where Value: ~Copyable {
  /// Dereferences the mutable reference allowing for in-place reads and writes
  /// to the underlying instance.
  @available(SwiftStdlib 6.3, *)
  @_alwaysEmitIntoClient
  public subscript() -> Value {
    @_transparent
    unsafeAddress {
      unsafe UnsafePointer(pointer)
    }
    
    @lifetime(copy self)
    @_transparent
    unsafeMutableAddress {
      unsafe pointer
    }
  }
}
