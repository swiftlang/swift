// With the CoroutineAccessors feature enabled, `_read`/`_modify` and
// `yielding borrow`/`yielding mutate` are two spellings of the same coroutine
// accessor and produce the *same* ABI.  This test uses the `_read`/`_modify`
// spelling; coroutine_accessors_yielding_synthesis.swift is the identical test
// with the `yielding borrow`/`yielding mutate` spelling.  The two should emit
// the same accessors in every case.
//
// The new yield_once_2 accessors are the primary implementation; the old
// yield_once accessors are emitted only additively when the module is resilient.

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

// Resilient: old yield_once accessors are additively emitted alongside the new.
// RESILIENT-DAG: sil{{.*}} @$s1m8StructRMV1iSivy : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8StructRMV1iSivr : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m8StructRMV1iSivx : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8StructRMV1iSivM : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m8globalRMSivy : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8globalRMSivr : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m8globalRMSivx : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m8globalRMSivM : $@yield_once @convention
