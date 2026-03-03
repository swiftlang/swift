//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Glibc

@usableFromInline
@_transparent
func errstr(_ no: Int32) -> String {
  unsafe withUnsafeTemporaryAllocation(of: CChar.self, capacity: Int(NL_TEXTMAX)) { buf in
    unsafe strerror_r(no, buf.baseAddress!, buf.count)
    return unsafe String(validatingCString: buf.baseAddress!)
      ?? "An unknown error occurred (\(no))."
  }
}

@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
@safe
public struct _MutexHandle: ~Copyable {
  @usableFromInline
  @safe
  let value: _Cell<pthread_mutex_t?>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    var mx = unsafe pthread_mutex_t(bitPattern: 0)
    let r = unsafe pthread_mutex_init(&mx, nil)
    if r != 0 {
      fatalError("couldn't initialize mutex: \(errstr(errno))")
    }
    value = unsafe _Cell(mx)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    let r = unsafe pthread_mutex_lock(value._address)
    if r != 0 {
      fatalError("couldn't lock mutex: \(errstr(errno))")
    }
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    unsafe pthread_mutex_trylock(value._address) == 0
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    let r = unsafe pthread_mutex_unlock(value._address)
    if r != 0 {
      fatalError("couldn't unlock mutex: \(errstr(errno))")
    }
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    let r = unsafe pthread_mutex_destroy(value._address)
    if r != 0 {
      fatalError("couldn't destroy mutex: \(errstr(errno))")
    }
  }
}
