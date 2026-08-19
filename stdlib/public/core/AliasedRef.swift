//===--- AliasedRef.swift -------------------------------------------------===//
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

/// A safe reference allowing reads of a single value that may be aliased.
///
/// `AliasedRef` is to `Ref` what `AliasedSpan` is to `Span`: it provides
/// lifetime safety for a reference to a single value, but does not depend on
/// the Law of Exclusivity, so the referenced value may be modified through
/// some other reference while this one is alive.
///
/// Unlike `Ref`, whose representation is opaque, an `AliasedRef` always stores
/// a pointer to the referenced value. Reading the value produces a copy, so
/// `Value` must be `Copyable`.
@frozen
@safe
@available(SwiftStdlib 6.5, *)
public struct AliasedRef<Value>: Copyable, ~Escapable, BitwiseCopyable {
  @usableFromInline
  internal let _pointer: UnsafePointer<Value>

  /// Initializes an instance of `AliasedRef` referring to the given borrowed
  /// value.
  ///
  /// Unlike `Ref`, creating an `AliasedRef` does not establish that the
  /// referenced value will not change: other references to the same storage
  /// may write to it while this reference is alive.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(borrow value)
  @_transparent
  public init(_ value: borrowing @_addressable Value) {
    let ref = unsafe AliasedRef(
      _unchecked: UnsafePointer(Builtin.unprotectedAddressOfBorrow(value))
    )
    self = unsafe _overrideLifetime(ref, borrowing: value)
  }

  /// Unsafely initializes an instance of `AliasedRef` using the given
  /// 'unsafeAddress' as the reference, based on the borrowed lifetime of the
  /// given 'owner' argument.
  ///
  /// - Parameter pointer: The address to use to reference an instance of
  ///                      type `Value`.
  /// - Parameter owner: The owning instance that this `AliasedRef` instance's
  ///                    lifetime is based on.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(borrow owner)
  @_transparent
  public init<Owner: ~Copyable & ~Escapable>(
    unsafeAddress pointer: UnsafePointer<Value>,
    borrowing owner: borrowing Owner
  ) {
    let ref = unsafe AliasedRef(_unchecked: pointer)
    self = unsafe _overrideLifetime(ref, borrowing: owner)
  }

  @unsafe
  @export(implementation)
  @inline(__always)
  @_lifetime(borrow pointer)
  internal init(_unchecked pointer: UnsafePointer<Value>) {
    unsafe _pointer = pointer
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedRef: @unchecked Sendable
where Value: Sendable & FullyInhabited {}

@available(SwiftStdlib 6.5, *)
extension AliasedRef {
  /// A copy of the referenced value.
  ///
  /// Unlike `Ref.value`, this property copies the value out of the referenced
  /// storage rather than borrowing it in place. The copy ensures that the
  /// returned value remains valid even if another reference to the same
  /// storage replaces it while the result of this access is still in use.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var value: Value {
    get {
      unsafe _pointer.pointee
    }
  }
}

// MARK: - conversions to and from `Ref`

@available(SwiftStdlib 6.5, *)
extension Ref {
  /// An aliased reference to a temporary copy of the referenced value.
  ///
  /// `Ref` does not necessarily store a pointer to its referent, whereas
  /// `AliasedRef` does. This accessor therefore copies the value into
  /// temporary storage and yields an `AliasedRef` referring to that copy,
  /// which remains valid for the duration of the access.
  //
  // FIXME: Spelled `_read` rather than `yielding borrow` because the
  // CoroutineAccessors experimental feature is not enabled for the standard
  // library. The two are the same accessor; switch the spelling once the
  // feature is on.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public var aliased: AliasedRef<Value> {
    @_lifetime(borrow self)
    _read {
      let copy = self.value
      yield unsafe AliasedRef(
        _unchecked: UnsafePointer(Builtin.addressOfBorrow(copy))
      )
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedRef {
  /// A reference to the same value as this aliased reference.
  ///
  /// Retrieving a `Ref` from an `AliasedRef` is an unsafe operation, because
  /// one must ensure that the underlying storage is not modified by any code
  /// while the ref (or any copy derived from it) is in use.
  @unsafe
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var ref: Ref<Value> {
    @_lifetime(copy self)
    get {
      let result = unsafe Ref(unsafeAddress: _pointer, borrowing: self)
      return unsafe _overrideLifetime(result, copying: self)
    }
  }
}
