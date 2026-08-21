// With the CoroutineAccessors feature enabled, `_read`/`_modify` and
// `yielding borrow`/`yielding mutate` are two spellings of the same coroutine
// accessor and produce the *same* ABI.  This test uses the `_read`/`_modify`
// spelling; coroutine_accessors_yielding_synthesis.swift is the identical test
// with the `yielding borrow`/`yielding mutate` spelling.  The two should emit
// the same accessors in every case.
//
// The new yield_once_2 accessors are the primary implementation; the old
// yield_once accessors are additionally emitted only when the module is
// resilient AND built for an ABI-stable platform (there is no prebuilt binary
// to stay compatible with otherwise).

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

public struct StructRM {
  var _s = 0
  public var i: Int {
    _read { yield _s }
    _modify { yield &_s }
  }
}

open class ClassRM {
  var _s = 0
  open var i: Int {
    _read { yield _s }
    _modify { yield &_s }
  }
}

var _g = 0
public var globalRM: Int {
  _read { yield _g }
  _modify { yield &_g }
}

// The new yield_once_2 accessors are always emitted (they are the primary
// implementation), including for the non-resilient module-scope global.
// FRAGILE-DAG:   sil{{.*}} @$s1m8StructRMV1iSivy : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m8StructRMV1iSivx : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m7ClassRMC1iSivy : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m7ClassRMC1iSivx : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m8globalRMSivy : $@yield_once_2
// FRAGILE-DAG:   sil{{.*}} @$s1m8globalRMSivx : $@yield_once_2

// Non-resilient: no old yield_once accessors at all (new ABI only), even though
// the property was written with `_read`/`_modify`.
// NO-OLD-ABI-NOT: @$s1m8StructRMV1iSivr
// NO-OLD-ABI-NOT: @$s1m8StructRMV1iSivM
// NO-OLD-ABI-NOT: @$s1m7ClassRMC1iSivr
// NO-OLD-ABI-NOT: @$s1m7ClassRMC1iSivM
// NO-OLD-ABI-NOT: @$s1m8globalRMSivr
// NO-OLD-ABI-NOT: @$s1m8globalRMSivM

// Resilient: old yield_once accessors are additively emitted alongside the new,
// but only on an ABI-stable platform.
// RESILIENT-DAG: sil{{.*}} @$s1m8StructRMV1iSivy : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8StructRMV1iSivx : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8globalRMSivy : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8globalRMSivx : $@yield_once_2
// RESILIENT-stable-DAG: sil{{.*}} @$s1m8StructRMV1iSivr : $@yield_once @convention
// RESILIENT-stable-DAG: sil{{.*}} @$s1m8StructRMV1iSivM : $@yield_once @convention
// RESILIENT-stable-DAG: sil{{.*}} @$s1m8globalRMSivr : $@yield_once @convention
// RESILIENT-stable-DAG: sil{{.*}} @$s1m8globalRMSivM : $@yield_once @convention

// NO-OLD-ABI-RESILIENT-unstable-NOT: @yield_once @convention

// On an ABI-stable platform this invocation has nothing new to verify (the
// other RESILIENT invocation above already confirms both ABIs there); just
// give the "stable" prefix a trivial match so FileCheck doesn't reject an
// entirely-unmatched --check-prefix.
// NO-OLD-ABI-RESILIENT-stable-DAG: sil{{.*}} @$s1m8StructRMV1iSivy : $@yield_once_2
