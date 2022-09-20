// RUN: %target-swift-frontend                \
// RUN:     -concurrency-model=task-to-thread \
// RUN:     -emit-sil                         \
// RUN:     -verify %s

// REQUIRES: freestanding

import _Concurrency

actor Simple {}

actor Custom { // expected-error{{not permitted within task-to-thread concurrency model}}
  let simple = Simple()

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    return simple.unownedExecutor
  }
}
