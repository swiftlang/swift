//===--- AliasedMutableRef.swift ------------------------------------------===//
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

/// A safe mutable reference allowing reads and writes of a single value that
/// may be aliased.
///
/// `AliasedMutableRef` is to `MutableRef` what `AliasedMutableSpan` is to
/// `MutableSpan`. It provides lifetime safety for a mutable reference to a
/// single value, but does not depend on the Law of Exclusivity, so the
/// referenced value may be read or written through some other reference while
/// this one is alive.
///
/// Because it already accounts for the presence of aliases,
/// `AliasedMutableRef` is `Copyable`, and its setter is non-mutating: storing
/// a value changes the referenced memory, not the reference. Reads and writes
/// copy the value, so `Value` must be `Copyable`.
@frozen
@safe
@available(SwiftStdlib 6.5, *)
public struct AliasedMutableRef<Value>: Copyable, ~Escapable, BitwiseCopyable {
  @usableFromInline
  internal let _pointer: UnsafeMutablePointer<Value>

  /// Initializes an instance of `AliasedMutableRef` referring to the given
  /// mutable value.
  ///
  /// Unlike `MutableRef`, creating an `AliasedMutableRef` does not establish
  /// exclusive access to the referenced value: other references to the same
  /// storage may read and write it while this reference is alive.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(&value)
  @_transparent
  public init(_ value: inout Value) {
    let ref = unsafe AliasedMutableRef(
      _unchecked: UnsafeMutablePointer(Builtin.unprotectedAddressOf(&value))
    )
    self = unsafe _overrideLifetime(ref, mutating: &value)
  }

  /// Unsafely initializes an instance of `AliasedMutableRef` using the given
  /// 'unsafeAddress' as the mutable reference, based on the mutating lifetime
  /// of the given 'owner' argument.
  ///
  /// - Parameter pointer: The address to use to mutably reference an instance
  ///                      of type `Value`.
  /// - Parameter owner: The owning instance that this `AliasedMutableRef`
  ///                    instance's lifetime is based on.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(&owner)
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress pointer: UnsafeMutablePointer<Value>,
    mutating owner: inout Owner
  ) {
    let ref = unsafe AliasedMutableRef(_unchecked: pointer)
    self = unsafe _overrideLifetime(ref, mutating: &owner)
  }

  @unsafe
  @export(implementation)
  @inline(__always)
  @_lifetime(borrow pointer)
  internal init(_unchecked pointer: UnsafeMutablePointer<Value>) {
    unsafe _pointer = pointer
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRef: @unchecked Sendable
where Value: Sendable & FullyInhabited {}

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRef {
  /// The referenced value.
  ///
  /// Unlike `MutableRef.value`, this property copies the value into and out
  /// of the referenced storage rather than providing direct access to it. The
  /// copy ensures that the accessed value remains valid even if another
  /// reference to the same storage replaces it concurrently.
  ///
  /// The setter is non-mutating because storing a value does not change the
  /// reference itself, only the contents of the storage it references.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var value: Value {
    get {
      unsafe _pointer.pointee
    }
    nonmutating set {
      unsafe _pointer.pointee = newValue
    }
  }
}

// MARK: - conversions

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRef {
  /// An aliased reference to the same value as this mutable reference.
  ///
  /// Retrieving a non-mutating aliased ref from an aliased mutable ref is a
  /// safe operation, because both already assume that the underlying storage
  /// may be aliased.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var aliased: AliasedRef<Value> {
    @_lifetime(copy self)
    get {
      let result = unsafe AliasedRef(_unchecked: UnsafePointer(_pointer))
      return unsafe _overrideLifetime(result, copying: self)
    }
  }

  /// A mutable reference to the same value as this aliased mutable
  /// reference.
  ///
  /// Retrieving a `MutableRef` from an `AliasedMutableRef` is an unsafe
  /// operation, because one must ensure that the underlying storage is not
  /// accessed at all (read or write) through any other reference while the
  /// mutable ref is in use.
  @unsafe
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var mutableRef: MutableRef<Value> {
    @_lifetime(copy self)
    get {
      var copyOfSelf = self
      let result = unsafe MutableRef(
        unsafeAddress: _pointer, mutating: &copyOfSelf
      )
      return unsafe _overrideLifetime(result, copying: self)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension MutableRef {
  /// Retrieve an aliased mutable ref from this mutable ref.
  ///
  /// This operation consumes the `MutableRef`, which ensures that the
  /// original reference (which assumes exclusivity) cannot be used while the
  /// returned `AliasedMutableRef`, or any copy of it, is still in use.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(copy self)
  @_transparent
  public consuming func asAliased() -> AliasedMutableRef<Value> {
    let result = unsafe AliasedMutableRef(_unchecked: pointer)
    return unsafe _overrideLifetime(result, copying: self)
  }
}
