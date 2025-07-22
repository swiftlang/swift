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

@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  @usableFromInline
  let value: _Cell<pthread_mutex_t?>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    var mx = pthread_mutex_t(bitPattern: 0)
    pthread_mutex_init(&mx, nil)
    value = _Cell(mx)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    pthread_mutex_lock(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    pthread_mutex_trylock(value._address) != 0
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    pthread_mutex_unlock(value._address)
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  deinit {
    pthread_mutex_destroy(value._address)
  }
}
