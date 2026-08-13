// Emission matrix for coroutine accessors under the CoroutineAccessors feature.
//
// Every coroutine accessor emits the new yield_once_2 ABI (vy/vx).  On an
// ABI-stable platform, the old yield_once ABI (vr/vM) is additionally emitted
// iff the storage is visible outside its module (public or package) AND
// resilient AND available before the feature's availability.  Otherwise only
// the new ABI is emitted.  This test exercises every combination of {public,
// package, internal} x {resilient, fragile} x {before-feature, at-feature}.
// (On a non-ABI-stable platform there is never an old binary to stay compatible
// with, so only the new ABI is ever emitted -- hence this requires an
// ABI-stable platform.)

// Resilient: public/package before-feature get old+new; everything else new-only.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-library-evolution -module-name m -package-name pkg \
// RUN:   | %FileCheck %s --check-prefix=RESILIENT
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-library-evolution -module-name m -package-name pkg \
// RUN:   | %FileCheck %s --check-prefix=RES-NEW-ONLY

// Fragile: no stable ABI to preserve, so everything is new-only.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m -package-name pkg                  \
// RUN:   | %FileCheck %s --check-prefix=FRAGILE
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m -package-name pkg                  \
// RUN:   | %FileCheck %s --check-prefix=FRAG-NEW-ONLY

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_stable_abi

public struct S {
  var _s = 0
  public var pub: Int { _read { yield _s } _modify { yield &_s } }
  package var pkg: Int { _read { yield _s } _modify { yield &_s } }
  internal var int_: Int { _read { yield _s } _modify { yield &_s } }
}

// SwiftStdlib 9999 availability is guaranteed to be later than the actual
// permanent availability of the new yielding accessor ABI.
@available(SwiftStdlib 9999, *)
public struct S9999 {
  var _s = 0
  public var pubNew: Int { _read { yield _s } _modify { yield &_s } }
  package var pkgNew: Int { _read { yield _s } _modify { yield &_s } }
}

// The new yield_once_2 accessors are always emitted, in every cell.
// RESILIENT-DAG: sil{{.*}} @$s1m1SV3pubSivy : $@yield_once_2 @convention
// RESILIENT-DAG: sil{{.*}} @$s1m1SV3pubSivx : $@yield_once_2 @convention
// RESILIENT-DAG: sil{{.*}} @$s1m1SV3pkgSivy : $@yield_once_2 @convention
// RESILIENT-DAG: sil{{.*}} @$s1m1SV3pkgSivx : $@yield_once_2 @convention
// RESILIENT-DAG: sil{{.*}} @$s1m1SV4int_Sivy : $@yield_once_2 @convention
// RESILIENT-DAG: sil{{.*}} @$s1m1SV4int_Sivx : $@yield_once_2 @convention
// RESILIENT-DAG: sil{{.*}} @$s1m5S9999V6pubNewSivy : $@yield_once_2 @convention
// RESILIENT-DAG: sil{{.*}} @$s1m5S9999V6pkgNewSivy : $@yield_once_2 @convention

// Old yield_once ABI: only public and package, before-feature (pub, pkg).
// RESILIENT-DAG: sil{{.*}} @$s1m1SV3pubSivr : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m1SV3pubSivM : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m1SV3pkgSivr : $@yield_once @convention
// RESILIENT-DAG: sil{{.*}} @$s1m1SV3pkgSivM : $@yield_once @convention

// ...and NOT for internal, nor for the at-feature (@available 9999) types.
// RES-NEW-ONLY-NOT: @$s1m1SV4int_Sivr
// RES-NEW-ONLY-NOT: @$s1m1SV4int_SivM
// RES-NEW-ONLY-NOT: @$s1m5S9999V6pubNewSivr
// RES-NEW-ONLY-NOT: @$s1m5S9999V6pubNewSivM
// RES-NEW-ONLY-NOT: @$s1m5S9999V6pkgNewSivr
// RES-NEW-ONLY-NOT: @$s1m5S9999V6pkgNewSivM

// Fragile: new ABI present everywhere...
// FRAGILE-DAG: sil{{.*}} @$s1m1SV3pubSivy : $@yield_once_2 @convention
// FRAGILE-DAG: sil{{.*}} @$s1m1SV3pkgSivy : $@yield_once_2 @convention
// FRAGILE-DAG: sil{{.*}} @$s1m5S9999V6pubNewSivy : $@yield_once_2 @convention

// ...and no old ABI anywhere, regardless of visibility or availability.
// FRAG-NEW-ONLY-NOT: $@yield_once @convention
