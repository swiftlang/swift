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
  internal var uBreakIterator: OpaquePointer
  internal var uText: OpaquePointer

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
  internal init(_uBreakIterator: OpaquePointer, _uText: OpaquePointer) {
    self.uBreakIterator = _uBreakIterator
    self.uText = _uText
  }

  // Get the current thread's TLS pointer. On first call for a given thread,
  // creates and initializes a new one.
  internal static func getPointer()
    -> UnsafeMutablePointer<_ThreadLocalStorage>
  {
    return _swift_stdlib_threadLocalStorageGet().assumingMemoryBound(
      to: _ThreadLocalStorage.self)
  }

  internal static func getUBreakIterator(
    _ bufPtr: UnsafeBufferPointer<UTF16.CodeUnit>
  ) -> OpaquePointer {
    let tlsPtr = getPointer()
    let brkIter = tlsPtr[0].uBreakIterator
    let utext = tlsPtr[0].uText

    var err = __swift_stdlib_U_ZERO_ERROR

    let start = bufPtr.baseAddress._unsafelyUnwrappedUnchecked
    _ = __swift_stdlib_utext_openUChars(
      utext, start, Int64(bufPtr.count), &err)
    _precondition(err.isSuccess, "Unexpected utext_openUChars failure")

    __swift_stdlib_ubrk_setUText(brkIter, utext, &err)
    _precondition(err.isSuccess, "Unexpected ubrk_setUText failure")

    return brkIter
  }

  internal static func getUBreakIterator(
    _ bufPtr: UnsafeBufferPointer<UTF8.CodeUnit>
  ) -> OpaquePointer {
    let tlsPtr = getPointer()
    let brkIter = tlsPtr[0].uBreakIterator
    let utext = tlsPtr[0].uText

    var err = __swift_stdlib_U_ZERO_ERROR

    let start = bufPtr.baseAddress._unsafelyUnwrappedUnchecked._asCChar
    _ = __swift_stdlib_utext_openUTF8(
      utext, start, Int64(bufPtr.count), &err)
    _precondition(err.isSuccess, "Unexpected utext_openUChars failure")

    __swift_stdlib_ubrk_setUText(brkIter, utext, &err)
    _precondition(err.isSuccess, "Unexpected ubrk_setUText failure")

    return brkIter
  }
}

// Destructor to register with pthreads. Responsible for deallocating any memory
// owned.
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

@_silgen_name("_stdlib_createTLS")
internal func _createThreadLocalStorage()
  -> UnsafeMutablePointer<_ThreadLocalStorage>
{
  // Allocate and initialize a UBreakIterator and UText.
  var err = __swift_stdlib_U_ZERO_ERROR
  let newUBreakIterator = __swift_stdlib_ubrk_open(
      /*type:*/ __swift_stdlib_UBRK_CHARACTER, /*locale:*/ nil,
      /*text:*/ nil, /*textLength:*/ 0, /*status:*/ &err)
  _precondition(err.isSuccess, "Unexpected ubrk_open failure")

  // utext_openUTF8 needs a valid pointer, even though we won't read from it
  var a: Int8 = 0x41
  let newUText = __swift_stdlib_utext_openUTF8(
      /*ut:*/ nil, /*s:*/ &a, /*len:*/ 1, /*status:*/ &err)

  _precondition(err.isSuccess, "Unexpected utext_openUTF8 failure")

  let tlsPtr: UnsafeMutablePointer<_ThreadLocalStorage>
    = UnsafeMutablePointer<_ThreadLocalStorage>.allocate(
      capacity: 1
  )
  tlsPtr.initialize(to: _ThreadLocalStorage(
    _uBreakIterator: newUBreakIterator, _uText: newUText))

  return tlsPtr
}
