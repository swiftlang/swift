// Companion to coroutine_accessors_read_synthesis.swift, using the
// `yielding borrow`/`yielding mutate` spelling instead of `_read`/`_modify`.
// Comparing the two reveals the residual spelling-dependent ABI gap: with this
// spelling the new yield_once_2 accessors are the *written* ones (always
// present), and the old yield_once_1 accessors appear only additively when the
// module is resilient AND built for an ABI-stable platform.

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=FRAGILE

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=NO-OLD-ABI

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefixes=RESILIENT,RESILIENT-%target-abi-stability

// On a non-ABI-stable platform, confirm the old accessors are still absent even
// under library evolution (separate invocation so a lone CHECK-NOT scans the
// whole input).  The NOT check below distinguishes old from new by the literal
// absence of "_2", so the new accessors' own convention must be pinned here too.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=NO-OLD-ABI-RESILIENT-%target-abi-stability

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

// Resilient: old yield_once_1 accessors are additively emitted alongside the
// new, but only on an ABI-stable platform.
// RESILIENT-DAG: sil{{.*}} @$s1m8StructYYV1iSivy : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8StructYYV1iSivx : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8globalYYSivy : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8globalYYSivx : $@yield_once_2
// RESILIENT-stable-DAG: sil{{.*}} @$s1m8StructYYV1iSivr : $@yield_once @convention
// RESILIENT-stable-DAG: sil{{.*}} @$s1m8StructYYV1iSivM : $@yield_once @convention
// RESILIENT-stable-DAG: sil{{.*}} @$s1m8globalYYSivr : $@yield_once @convention
// RESILIENT-stable-DAG: sil{{.*}} @$s1m8globalYYSivM : $@yield_once @convention

// NO-OLD-ABI-RESILIENT-unstable-NOT: @yield_once @convention

// On an ABI-stable platform this invocation has nothing new to verify (the
// other RESILIENT invocation above already confirms both ABIs there); just
// give the "stable" prefix a trivial match so FileCheck doesn't reject an
// entirely-unmatched --check-prefix.
// NO-OLD-ABI-RESILIENT-stable-DAG: sil{{.*}} @$s1m8StructYYV1iSivy : $@yield_once_2
