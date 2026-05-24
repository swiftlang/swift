// RUN: %target-swift-frontend -emit-silgen -target %target-future-triple -swift-version 6 -module-name main %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: objc_interop

// rdar://175957720
//
// For a global-actor-isolated `isolated deinit` on an @objc class, the native
// `__deallocating_deinit` already hops to the expected executor via
// `swift_task_deinitOnExecutor`. The @objc thunk (invoked by ObjC `release`
// from any thread) must NOT emit `_checkExpectedExecutor`: releasing the
// last reference from a non-isolated context would otherwise crash in
// `_dispatch_assert_queue_fail`.

import Foundation

@MainActor
@objc final class MainActorIsolated: NSObject {
  nonisolated override init() {}
  isolated deinit {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s4main17MainActorIsolatedCfDTo
// CHECK-NOT: _checkExpectedExecutor
// CHECK: end sil function '$s4main17MainActorIsolatedCfDTo'

// Non-deinit @objc methods on the same class must continue to receive the
// precondition check in their thunk — isolation enforcement for ordinary
// method dispatch is unchanged.
extension MainActorIsolated {
  @objc func method() {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s4main17MainActorIsolatedC6methodyyFTo
// CHECK: _checkExpectedExecutor
// CHECK: end sil function '$s4main17MainActorIsolatedC6methodyyFTo'

