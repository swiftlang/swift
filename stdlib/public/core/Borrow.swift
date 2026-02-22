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

/// A safe reference allowing in-place reads to a shared value.
@available(SwiftStdlib 6.4, *)
@frozen
public struct Borrow<Value: ~Copyable>: Copyable, ~Escapable {
  @usableFromInline
  let builtin: Builtin.Borrow<Value>

  /// Initializes an instance of `Borrow` with the given borrowed value. This
  /// creates a constant reference to that value preventing writes on the 
  /// original value while this reference is still active.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_lifetime(borrow value)
  @_transparent
  public init(_ value: borrowing Value) {
    builtin = Builtin.makeBorrow(value)
  }

  /// Unsafely initializes an instance of `Borrow` using the given
  /// 'unsafeAddress' as the reference based on the borrowed lifetime of the
  /// given 'owner' argument.
  ///
  /// - Parameter unsafeAddress: The address to use to reference an instance of
  ///                            type `Value`.
  /// - Parameter owner: The owning instance that this `Borrow` instance's
  ///                    lifetime is based on.
  @available(SwiftStdlib 6.4, *)
  @unsafe
  @_alwaysEmitIntoClient
  @_lifetime(borrow owner)
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress pointer: UnsafePointer<Value>,
    borrowing owner: borrowing Owner
  ) {
    builtin = unsafe Builtin.makeBorrow(pointer.pointee)
  }
}

@available(SwiftStdlib 6.4, *)
extension Borrow where Value: ~Copyable {
  /// Dereferences the constant reference allowing for in-place reads to the
  /// underlying value.
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var value: Value {
    borrow {
      Builtin.dereferenceBorrow(builtin)
    }
  }
}
