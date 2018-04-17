//===--- ThreadLocalStorage.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

// For testing purposes, a thread-safe counter to guarantee that destructors get
// called by pthread.
#if INTERNAL_CHECKS_ENABLED
public // @testable
let _destroyTLSCounter = _stdlib_AtomicInt()
#endif

// Thread local storage for all of the Swift standard library
//
// @moveonly/@pointeronly: shouldn't be used as a value, only through its
// pointer. Similarly, shouldn't be created, except by
// _initializeThreadLocalStorage.
//
@usableFromInline // FIXME(sil-serialize-all)
@_fixed_layout // FIXME(sil-serialize-all)
internal struct _ThreadLocalStorage {
  // TODO: might be best to absract uBreakIterator handling and caching into
  // separate struct. That would also make it easier to maintain multiple ones
  // and other TLS entries side-by-side.

  // Save a pre-allocated UBreakIterator, as they are very expensive to set up.
  // Each thread can reuse their unique break iterator, being careful to reset
  // the text when it has changed (see below). Even with a naive always-reset
  // policy, grapheme breaking is 30x faster when using a pre-allocated
  // UBreakIterator than recreating one.
  //
  // private
  @usableFromInline // FIXME(sil-serialize-all)
  internal var uBreakIterator: OpaquePointer

  // TODO: Consider saving two, e.g. for character-by-character comparison

  // The below cache key tries to avoid resetting uBreakIterator's text when
  // operating on the same String as before. Avoiding the reset gives a 50%
  // speedup on grapheme breaking.
  //
  // As a invalidation check, save the base address from the last used
  // StringCore. We can skip resetting the uBreakIterator's text when operating
  // on a given StringCore when both of these associated references/pointers are
  // equal to the StringCore's. Note that the owner is weak, to force it to
  // compare unequal if a new StringCore happens to be created in the same
  // memory.
  //
  // TODO: unowned reference to string owner, base address, and _countAndFlags

  // private: Should only be called by _initializeThreadLocalStorage
  @inlinable // FIXME(sil-serialize-all)
  internal init(_uBreakIterator: OpaquePointer) {
    self.uBreakIterator = _uBreakIterator
  }

  // Get the current thread's TLS pointer. On first call for a given thread,
  // creates and initializes a new one.
  @inlinable // FIXME(sil-serialize-all)
  internal static func getPointer()
    -> UnsafeMutablePointer<_ThreadLocalStorage>
  {
    let tlsRawPtr = _stdlib_thread_getspecific(_tlsKey)
    if _fastPath(tlsRawPtr != nil) {
      return tlsRawPtr._unsafelyUnwrappedUnchecked.assumingMemoryBound(
        to: _ThreadLocalStorage.self)
    }

    return _initializeThreadLocalStorage()
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func getUBreakIterator(
    start: UnsafePointer<UTF16.CodeUnit>,
    count: Int32
  ) -> OpaquePointer {
    let tlsPtr = getPointer()
    let brkIter = tlsPtr[0].uBreakIterator

    var err = __swift_stdlib_U_ZERO_ERROR
    __swift_stdlib_ubrk_setText(brkIter, start, count, &err)
    _precondition(err.isSuccess, "Unexpected ubrk_setUText failure")

    return brkIter
  }
}

// Destructor to register with pthreads. Responsible for deallocating any memory
// owned.
@usableFromInline // FIXME(sil-serialize-all)
@_silgen_name("_stdlib_destroyTLS")
internal func _destroyTLS(_ ptr: UnsafeMutableRawPointer?) {
  _sanityCheck(ptr != nil,
    "_destroyTLS was called, but with nil...")
  let tlsPtr = ptr!.assumingMemoryBound(to: _ThreadLocalStorage.self)
  __swift_stdlib_ubrk_close(tlsPtr[0].uBreakIterator)
  tlsPtr.deinitialize(count: 1)
  tlsPtr.deallocate()

#if INTERNAL_CHECKS_ENABLED
  // Log the fact we've destroyed our storage
  _destroyTLSCounter.fetchAndAdd(1)
#endif
}

// Lazily created global key for use with pthread TLS
@usableFromInline // FIXME(sil-serialize-all)
internal let _tlsKey: __swift_thread_key_t = {
  let sentinelValue = __swift_thread_key_t.max
  var key: __swift_thread_key_t = sentinelValue
  let success = _stdlib_thread_key_create(&key, _destroyTLS)
  _sanityCheck(success == 0, "somehow failed to create TLS key")
  _sanityCheck(key != sentinelValue, "Didn't make a new key")
  return key
}()

@inlinable // FIXME(sil-serialize-all)
@inline(never)
internal func _initializeThreadLocalStorage()
  -> UnsafeMutablePointer<_ThreadLocalStorage>
{
  _sanityCheck(_stdlib_thread_getspecific(_tlsKey) == nil,
    "already initialized")

  // Create and initialize one.
  var err = __swift_stdlib_U_ZERO_ERROR
  let newUBreakIterator = __swift_stdlib_ubrk_open(
      /*type:*/ __swift_stdlib_UBRK_CHARACTER, /*locale:*/ nil,
      /*text:*/ nil, /*textLength:*/ 0, /*status:*/ &err)
  _precondition(err.isSuccess, "Unexpected ubrk_open failure")

  let tlsPtr: UnsafeMutablePointer<_ThreadLocalStorage>
    = UnsafeMutablePointer<_ThreadLocalStorage>.allocate(
      capacity: 1
  )
  tlsPtr.initialize(
    to: _ThreadLocalStorage(_uBreakIterator: newUBreakIterator)
  )
  let success = _stdlib_thread_setspecific(_tlsKey, tlsPtr)
  _sanityCheck(success == 0, "setspecific failed")
  return tlsPtr
}
