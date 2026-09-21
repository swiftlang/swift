// When the CoroutineAccessors feature is enabled, the old (yield_once_1)
// `_read`/`_modify` coroutine accessors are emitted *only* to preserve a stable
// ABI. A module that is not built with library evolution has no stable ABI to
// keep, so only the new callee-allocated (yield_once_2) accessors are emitted.
// Under library evolution, an ABI-stable platform additionally emits the old
// accessors; a non-ABI-stable platform has no prebuilt binary to stay
// compatible with, so it still emits only the new accessors.

// Non-resilient: the new yield_once_2 accessors are present...
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -module-name main                                 \
// RUN:   | %FileCheck %s --check-prefix=FRAGILE

// ...and the old yield_once accessors are absent (separate invocation so the
// CHECK-NOT scans the whole input regardless of accessor emission order).
// The new accessors' own convention is pinned explicitly: the NOT check below
// distinguishes old from new by the literal absence of "_2", so it needs the
// new accessors to actually use the yield_once_2 convention here too.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -module-name main                                 \
// RUN:   | %FileCheck %s --check-prefix=NOOLD

// Resilient: the new accessors are always present; the old accessors are
// additionally present only on an ABI-stable platform.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name main                                 \
// RUN:   | %FileCheck %s --check-prefixes=RESILIENT,RESILIENT-%target-abi-stability

// On a non-ABI-stable platform, confirm the old accessors are still absent even
// under library evolution (separate invocation, as above, so a lone CHECK-NOT
// scans the whole input; same reason for pinning the convention explicitly).
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name main                                 \
// RUN:   | %FileCheck %s --check-prefix=NOOLD-RESILIENT-%target-abi-stability

// REQUIRES: swift_feature_CoroutineAccessors

public struct S {
  var _i: Int = 0
  public var i: Int {
    yielding borrow { yield _i }
    yielding mutate { yield &_i }
  }
}

// FRAGILE-DAG: sil {{.*}}Sivy : $@yield_once_2 @convention
// FRAGILE-DAG: sil {{.*}}Sivx : $@yield_once_2 @convention

// NOOLD-NOT: @yield_once @convention

// RESILIENT-DAG: sil {{.*}}Sivy : $@yield_once_2 @convention
// RESILIENT-DAG: sil {{.*}}Sivx : $@yield_once_2 @convention
// RESILIENT-stable-DAG: sil {{.*}}Sivr : $@yield_once @convention
// RESILIENT-stable-DAG: sil {{.*}}SivM : $@yield_once @convention

// NOOLD-RESILIENT-unstable-NOT: @yield_once @convention

// On an ABI-stable platform this invocation has nothing new to verify (the
// other RESILIENT invocation above already confirms both ABIs there); just
// give the "stable" prefix a trivial match so FileCheck doesn't reject an
// entirely-unmatched --check-prefix.
// NOOLD-RESILIENT-stable-DAG: sil {{.*}}Sivy : $@yield_once_2 @convention
