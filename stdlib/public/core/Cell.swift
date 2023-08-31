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

// FIXME: This should be UnsafeCell<T: ~Copyable>: ~Copyable {}

/// An unsafe container that holds a value of type 'T'.
///
/// This type is unsafe because it vends an unsafe address to the underlying
/// value which may change upon consumption of the cell.
@available(SwiftStdlib 5.10, *)
@_rawLayout(like: T)
@frozen
public struct UnsafeCell<T>: ~Copyable {
  /// The unsafe stable address of the value within this cell.
  ///
  /// Note: this address is only known to be stable while borrowing the cell.
  /// Within a consuming context the cell may move to new location invalidating
  /// any previous pointer vended by the cell.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public var address: UnsafeMutablePointer<T> {
    UnsafeMutablePointer<T>(Builtin.unprotectedAddressOfBorrow(self))
  }

  /// Initializes a value of this unsafe cell with the memory being zeroed out.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public init() {}

  /// Initializes a value of this unsafe cell with the given initial value.
  ///
  /// - Parameter initialValue: The initial value to set this unsafe cell.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public init(_ initialValue: consuming T) {
    address.initialize(to: initialValue)
  }

  @inlinable
  deinit {
    address.deinitialize(count: 1)
  }
}

// FIXME: This should be Cell<T: ~Copyable>: ~Copyable {}
// FIXME: Better explanation of this type?

/// A container that holds a value of type 'T'.
///
/// This type also affords interior mutability which means that in non-mutable
/// contexts, i.e. say a borrowing function, we're able to mutate the underlying
/// value. We have ownership of the value being contained as well as having a
/// stable reference to it until we're consumed at which point the cell may move
/// to a new location.
@available(SwiftStdlib 5.10, *)
@frozen
public struct Cell<T>: ~Copyable {
  @usableFromInline
  let _value: UnsafeCell<T>

  /// Initializes a value of this cell with the given initial value.
  ///
  /// - Parameter initialValue: The initial value to set this cell.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public init(_ initialValue: consuming T) {
    _value = UnsafeCell<T>(initialValue)
  }

  // Define a deinit to appease the compiler to accept our 'discard self'. This
  // deinit won't do anything, but we will still implicitly destroy 'value'
  // which will deinitialize the value being stored.
  @inlinable
  deinit {}
}

@available(SwiftStdlib 5.10, *)
extension Cell {
  /// Replaces the current value within the cell with the given replacement
  /// value and returns the old value.
  ///
  /// - Parameter replacement: A new value to store within the cell.
  /// - Returns: The old replaced value.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public borrowing func replace(with replacement: consuming T) -> T {
    let existing = _value.address.move()
    _value.address.initialize(to: replacement)
    return existing
  }

  /// Sets the value within the cell to the given replacement value and consumes
  /// the old value.
  ///
  /// - Parameter replacement: A new value to store within the cell.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public borrowing func set(_ replacement: consuming T) {
    let existing = replace(with: replacement)
    _ = consume existing
  }

  /// Consumes the cell and returns the value that was stored within it.
  ///
  /// This returns ownership of the held on value back to the caller of this
  /// take.
  ///
  /// - Returns: The value within the cell.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public consuming func take() -> T {
    let existing = _value.address.move()
    discard self
    return existing
  }
}

@available(SwiftStdlib 5.10, *)
extension Cell /* where T: Copyable */ {
  /// Creates a copy of the value being stored.
  ///
  /// - Returns: A copied version of the value.
  @available(SwiftStdlib 5.10, *)
  @inlinable
  public var value: T {
    _value.address.pointee
  }
}
