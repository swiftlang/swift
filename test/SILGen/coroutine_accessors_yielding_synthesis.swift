// Companion to coroutine_accessors_read_synthesis.swift, using the
// `yielding borrow`/`yielding mutate` spelling instead of `_read`/`_modify`.
// Comparing the two reveals the residual spelling-dependent ABI gap: with this
// spelling the new yield_once_2 accessors are the *written* ones (always
// present), and the old yield_once_1 accessors appear only additively when the
// module is resilient.

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=FRAGILE

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=NO-OLD-ABI

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=RESILIENT

// REQUIRES: swift_feature_CoroutineAccessors

public struct StructYY {
  var _s = 0
  public var i: Int {
    yielding borrow { yield _s }
    yielding mutate { yield &_s }
  }
}

open class ClassYY {
  var _s = 0
  open var i: Int {
    yielding borrow { yield _s }
    yielding mutate { yield &_s }
  }
}

var _g = 0
public var globalYY: Int {
  yielding borrow { yield _g }
  yielding mutate { yield &_g }
}

// The new yield_once_2 accessors are always emitted (they are the written
// accessors) -- including for the non-resilient module-scope global, unlike the
// `_read` spelling where that global gets *no* opaque coroutines.
// FRAGILE-DAG:   sil{{.*}} @$s1m8StructYYV1iSivy : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m8StructYYV1iSivx : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m7ClassYYC1iSivy : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m7ClassYYC1iSivx : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m8globalYYSivy : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m8globalYYSivx : $@yield_once_2

// Non-resilient: no old yield_once_1 accessors at all (new ABI only).
// NO-OLD-ABI-NOT: @$s1m8StructYYV1iSivr
// NO-OLD-ABI-NOT: @$s1m8StructYYV1iSivM
// NO-OLD-ABI-NOT: @$s1m7ClassYYC1iSivr
// NO-OLD-ABI-NOT: @$s1m7ClassYYC1iSivM
// NO-OLD-ABI-NOT: @$s1m8globalYYSivr
// NO-OLD-ABI-NOT: @$s1m8globalYYSivM

// Resilient: old yield_once_1 accessors are additively emitted alongside the new.
// RESILIENT-DAG: sil{{.*}} @$s1m8StructYYV1iSivy : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8StructYYV1iSivr : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m8StructYYV1iSivx : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8StructYYV1iSivM : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m8globalYYSivy : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8globalYYSivr : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m8globalYYSivx : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8globalYYSivM : $@yield_once @convention
