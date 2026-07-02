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

import Builtin

@available(SwiftStdlib 6.4, *)
@frozen
@safe
public struct WeakBox<Value: ~Copyable>: ~Copyable {
  @usableFromInline
  let pointer: UnsafeMutablePointer<SharedBox<Value>._Storage>

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  init(_ other: UnsafeMutablePointer<SharedBox<Value>._Storage>) {
    unsafe pointer = other
  }

  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    // Perform a relaxed load first. This is a small optimization to elide the
    // atomic rmw in the unique case. If we find that there is a single weak
    // user, then it means this weak instance is the last reference to this
    // memory.
    //
    // See: (https://github.com/gcc-mirror/gcc/commit/dbf8bd3c2f2cd2d27ca4f0fe379bd9490273c6d7)
    let refCount = unsafe pointer.pointee.refCount.load(ordering: .relaxed)

    if refCount & SHARED_WEAK_MASK == SHARED_WEAK_INIT {
      unsafe pointer.deallocate()
      return
    }

    // Otherwise, there was another weak user. Perform the rmw to decrement our
    // count.
    //
    // FIXME: I would like to write 'storage.value.decrementWeak()'
    let (ov, _) = unsafe pointer.pointee.refCount.wrappingSubtract(
      SHARED_WEAK_ADD,
      ordering: .relaxed
    )

    // Nothing more to do if we weren't the last weak user.
    if ov & SHARED_WEAK_MASK != SHARED_WEAK_INIT {
      return
    }

    // We are the last weak reference, time to free this memory.
    unsafe pointer.deallocate()
  }
}

@available(SwiftStdlib 6.4, *)
extension WeakBox where Value: ~Copyable {
  // Note: This is safe because 'SharedBox' will always have at least shared access to
  //       the storage allocation which has already been initialized.
  @_alwaysEmitIntoClient
  @_transparent
  internal var storage: Borrow<SharedBox<Value>._Storage> {
    unsafe Borrow(unsafeAddress: pointer, borrowing: self)
  }
}

@available(SwiftStdlib 6.4, *)
extension WeakBox where Value: ~Copyable {
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var strongCount: Int {
    Int(storage.value.refCount.load(ordering: .relaxed) & SHARED_STRONG_MASK)
  }

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var weakCount: Int {
    let masked = storage.value.refCount.load(ordering: .relaxed) & SHARED_WEAK_MASK
    return Int(masked &>> SHARED_WEAK_SHIFT)
  }
}

@available(SwiftStdlib 6.4, *)
extension WeakBox where Value: ~Copyable {
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func clone() -> WeakBox<Value> {
    storage.value.incrementWeak()
    return unsafe WeakBox(pointer)
  }

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public func promote() -> SharedBox<Value>? {
    var result = (
      exchanged: false,
      original: storage.value.refCount.load(ordering: .relaxed)
    )

    repeat {
      // If we see that our strong count is already 0, then the value has already
      // been deinitialized.
      if result.original & SHARED_STRONG_MASK == 0 {
        return nil
      }

      // This promotion would've overflowed the strong count, bail.
      if result.original & SHARED_STRONG_MASK == SHARED_STRONG_MASK {
        return nil
      }

      result = storage.value.refCount.weakCompareExchange(
        expected: result.original,
        desired: result.original &+ SHARED_STRONG_ADD,
        successOrdering: .acquiring,
        failureOrdering: .relaxed
      )
    } while !result.exchanged

    return unsafe SharedBox(pointer)
  }
}
