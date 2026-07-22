// When the CoroutineAccessors feature is enabled, the old (yield_once_1)
// `_read`/`_modify` coroutine accessors are emitted *only* to preserve a stable
// ABI. A module that is not built with library evolution has no stable ABI to
// keep, so only the new callee-allocated (yield_once_2) accessors are emitted.
// Under library evolution both the old and new accessors are emitted.

// Non-resilient: the new yield_once_2 accessors are present...
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name main                                 \
// RUN:   | %FileCheck %s --check-prefix=FRAGILE

// ...and the old yield_once accessors are absent (separate invocation so the
// CHECK-NOT scans the whole input regardless of accessor emission order).
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name main                                 \
// RUN:   | %FileCheck %s --check-prefix=NOOLD

// Resilient: both the old and new accessors are present.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name main                                 \
// RUN:   | %FileCheck %s --check-prefix=RESILIENT

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

// RESILIENT-DAG: sil {{.*}}Sivr : $@yield_once @convention
// RESILIENT-DAG: sil {{.*}}Sivy : $@yield_once_2 @convention
// RESILIENT-DAG: sil {{.*}}SivM : $@yield_once @convention
// RESILIENT-DAG: sil {{.*}}Sivx : $@yield_once_2 @convention
