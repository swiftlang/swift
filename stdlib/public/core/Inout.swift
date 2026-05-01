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

/// A safe mutable reference allowing in-place mutation to an exclusive value.
@available(SwiftStdlib 6.4, *)
@frozen
@safe
public struct Inout<Value: ~Copyable>: ~Copyable, ~Escapable {
  @usableFromInline
  let pointer: UnsafeMutablePointer<Value>

  /// Initializes an instance of `Inout` with the given mutable value. This
  /// creates a mutable reference to that value preventing writes to the
  /// original value while this mutable reference is still active.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_lifetime(&value)
  @_transparent
  public init(_ value: inout Value) {
    unsafe pointer = UnsafeMutablePointer(Builtin.unprotectedAddressOf(&value))
  }

  /// Unsafely initializes an instance of `Inout` using the given
  /// 'unsafeAddress' as the mutable reference based on the mutating lifetime of
  /// the given 'owner' argument.
  ///
  /// - Parameter unsafeAddress: The address to use to mutably reference an
  ///                            instance of type `Value`.
  /// - Parameter owner: The owning instance that this `Inout` instance's
  ///                    lifetime is based on.
  @available(SwiftStdlib 6.4, *)
  @unsafe
  @_alwaysEmitIntoClient
  @_lifetime(&owner)
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress pointer: UnsafeMutablePointer<Value>,
    mutating owner: inout Owner
  ) {
    unsafe self.pointer = pointer
  }
}

@available(SwiftStdlib 6.4, *)
extension Inout where Value: ~Copyable {
  /// Dereferences the mutable reference allowing for in-place reads and writes
  /// to the underlying value.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var value: Value {
    @_unsafeSelfDependentResult
    borrow {
      unsafe pointer.pointee
    }

    @_unsafeSelfDependentResult
    mutate {
      unsafe &pointer.pointee
    }
  }
}
