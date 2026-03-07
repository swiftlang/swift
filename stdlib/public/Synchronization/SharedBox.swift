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

@_alwaysEmitIntoClient
@_transparent
var SHARED_REFCOUNT_INIT: UInt {
  // On initialization, start both weak and strong ref counts at 1. When the
  // strong count hits 0, it is responsible for deinitializing the memory while
  // when the weak count hits 0, it will deallocate the data.
  SHARED_WEAK_INIT &+ SHARED_STRONG_INIT
}

@_alwaysEmitIntoClient
@_transparent
var SHARED_STRONG_MASK: UInt {
  0xFFFFFFFF
}

@_alwaysEmitIntoClient
@_transparent
var SHARED_STRONG_INIT: UInt {
  1 &<< 0
}

@_alwaysEmitIntoClient
@_transparent
var SHARED_STRONG_ADD: UInt {
  SHARED_STRONG_INIT
}

@_alwaysEmitIntoClient
@_transparent
var SHARED_WEAK_SHIFT: UInt {
  // This division should be constant folded.
  UInt(truncatingIfNeeded: UInt.bitWidth / 2)
}

@_alwaysEmitIntoClient
@_transparent
var SHARED_WEAK_MASK: UInt {
  SHARED_STRONG_MASK &<< SHARED_WEAK_SHIFT
}

@_alwaysEmitIntoClient
@_transparent
var SHARED_WEAK_INIT: UInt {
  1 &<< SHARED_WEAK_SHIFT
}

@_alwaysEmitIntoClient
@_transparent
var SHARED_WEAK_ADD: UInt {
  SHARED_WEAK_INIT
}

@available(SwiftStdlib 6.4, *)
extension SharedBox where Value: ~Copyable {
  @available(SwiftStdlib 6.4, *)
  @frozen
  @usableFromInline
  struct _Storage: ~Copyable {
    @usableFromInline
    let refCount = Atomic<UInt>(SHARED_REFCOUNT_INIT)

    @usableFromInline
    var value: Value

    @available(SwiftStdlib 6.4, *)
    @_alwaysEmitIntoClient
    @_transparent
    init(_ initialValue: consuming Value) {
      value = initialValue
    }
  }
}

@available(SwiftStdlib 6.4, *)
extension SharedBox._Storage where Value: ~Copyable {
  @_alwaysEmitIntoClient
  func incrementStrong() {
    let (ov, _) = refCount.wrappingAdd(SHARED_STRONG_ADD, ordering: .relaxed)

    if ov & SHARED_STRONG_MASK == SHARED_STRONG_MASK {
      Builtin.int_trap()
    }
  }

  @_alwaysEmitIntoClient
  func decrementStrong() -> (oldValue: UInt, newValue: UInt) {
    refCount.wrappingSubtract(SHARED_STRONG_ADD, ordering: .releasing)
  }

  @_alwaysEmitIntoClient
  func incrementWeak() {
    let (ov, _) = refCount.wrappingAdd(SHARED_WEAK_ADD, ordering: .relaxed)

    if ov & SHARED_WEAK_MASK == SHARED_WEAK_MASK {
      Builtin.int_trap()
    }
  }

  @_alwaysEmitIntoClient
  func decrementWeak() -> (oldValue: UInt, newValue: UInt) {
    refCount.wrappingSubtract(SHARED_WEAK_ADD, ordering: .relaxed)
  }
}

@available(SwiftStdlib 6.4, *)
@frozen
@safe
public struct SharedBox<Value: ~Copyable>: ~Copyable {
  @usableFromInline
  let pointer: UnsafeMutablePointer<_Storage>

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public init(_ initialValue: consuming Value) {
    unsafe pointer = .allocate(capacity: 1)
    unsafe pointer.initialize(to: _Storage(initialValue))
  }

  @available(SwiftStdlib 6.4, *)
  @unsafe
  @_alwaysEmitIntoClient
  @_transparent
  init(_ other: UnsafeMutablePointer<_Storage>) {
    unsafe pointer = other
  }

  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    // Perform an acquire load first. This is a small optimization to elide the
    // atomic rmw in the unique case. If we find that there is a single strong
    // user and a single weak user, then it means this shared instance was the
    // sole unique owner. We can deinitialize and deallocate the memory in one
    // sweep here.
    //
    // See: (https://github.com/gcc-mirror/gcc/commit/dbf8bd3c2f2cd2d27ca4f0fe379bd9490273c6d7)
    let refCount = unsafe pointer.pointee.refCount.load(ordering: .acquiring)

    if refCount == SHARED_REFCOUNT_INIT {
      unsafe pointer.deinitialize(count: 1)
      unsafe pointer.deallocate()
      return
    }

    // Otherwise, there was another weak or strong user. Perform the rmw to
    // decrement our count.
    //
    // FIXME: I would like to write 'storage.value.decrementStrong()' here
    let (ov, _) = unsafe pointer.pointee.refCount.wrappingSubtract(
      SHARED_STRONG_ADD,
      ordering: .releasing
    )

    // Nothing more to do if we weren't the last strong user.
    if ov & SHARED_STRONG_MASK != SHARED_STRONG_INIT {
      return
    }

    atomicMemoryFence(ordering: .acquiring)

    unsafe pointer.deinitialize(count: 1)

    // We let our implicit weak reference deallocate the storage if we're the
    // last weak owner. Otherwise, some other weak instance will get rid of it.
    let _ = unsafe WeakBox(pointer)
  }
}

@available(SwiftStdlib 6.4, *)
extension SharedBox where Value: ~Copyable {
  // Note: This is safe because 'SharedBox' will always have at least shared access to
  //       the storage allocation which has already been initialized.
  @_alwaysEmitIntoClient
  @_transparent
  internal var storage: Borrow<_Storage> {
    @_lifetime(borrow self)
    borrowing get {
      unsafe Borrow(unsafeAddress: pointer, borrowing: self)
    }
  }

  @_alwaysEmitIntoClient
  @_transparent
  internal var valuePtr: UnsafeMutablePointer<Value> {
    // _Storage's first word is both the strong and weak ref count, so offset by
    // a word to get the pointer to the value.
    let offsetPtr = unsafe UnsafeRawPointer(pointer) + MemoryLayout<Int>.size
    return unsafe UnsafeMutablePointer<Value>(offsetPtr._rawValue)
  }
}

@available(SwiftStdlib 6.4, *)
extension SharedBox where Value: ~Copyable {
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var isUnique: Bool {
    storage.value.refCount.load(ordering: .acquiring) & SHARED_STRONG_MASK == SHARED_STRONG_INIT
  }

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

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var value: Value {
    @_unsafeSelfDependentResult
    borrow {
      unsafe pointer.pointee.value
    }
  }
}

@available(SwiftStdlib 6.4, *)
extension SharedBox where Value: ~Copyable {
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  @_transparent
  public func borrow() -> Borrow<Value> {
    unsafe Borrow(unsafeAddress: UnsafePointer(valuePtr), borrowing: self)
  }

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public func clone() -> SharedBox<Value> {
    storage.value.incrementStrong()
    return unsafe SharedBox(pointer)
  }

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public func demote() -> WeakBox<Value> {
    storage.value.incrementWeak()
    return unsafe WeakBox(pointer)
  }

  @_alwaysEmitIntoClient
  public consuming func consume() -> Value? {
    // Make sure not to run our deinit regardless of if we're the last user or
    // not.
    discard self

    // Decrease the strong count. If the old value was not 1, then there are
    // other strong users.
    let (ov, _) = storage.value.decrementStrong()

    if ov & SHARED_STRONG_MASK != SHARED_STRONG_INIT {
      return nil
    }

    // Otherwise, we were the last strong reference and can return the value out.
    atomicMemoryFence(ordering: .acquiring)

    let value = valuePtr.move()

    // We let our implicit weak reference deallocate the storage if we're the
    // last weak owner. Otherwise, some other weak instance will get rid of it.
    let _ = unsafe WeakBox(pointer)

    return value
  }

  @available(SwiftStdlib 6.4, *)
  @discardableResult
  @_alwaysEmitIntoClient
  public mutating func ensureUnique(
    cloner: (borrowing Value) -> Value
  ) -> Bool {
    if isUnique {
      return true
    }

    replace(using: cloner)
    return false
  }

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func replace(using body: (borrowing Value) -> Value) {
    self = SharedBox(body(value))
  }

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func edit(
    shared cloner: (borrowing Value) -> Value,
    unique updater: (inout Value) -> ()
  ) {
    if !isUnique {
      replace(using: cloner)
    }

    unsafe updater(&pointer.pointee.value)
  }

  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func withValue<E: Error>(
    _ body: (inout Value) throws(E) -> ()
  ) throws(E) -> Bool {
    if isUnique {
      unsafe try body(&pointer.pointee.value)
      return true
    }

    return false
  }
}

@available(SwiftStdlib 6.4, *)
extension SharedBox /* where Value: Clonable */ {
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public mutating func makeUnique() {
    ensureUnique {
      $0
    }
  }
}
