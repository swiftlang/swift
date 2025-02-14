// RUN: %target-swift-emit-silgen -module-name accessor_borrow \
// RUN: -enable-experimental-feature LifetimeDependence \
// RUN: %s | %FileCheck %s

// REQUIRES: swift_feature_LifetimeDependence

struct NE: ~Escapable {}

struct NEContainer: ~Copyable {
  // This accessor borrows self. Do not synthesize a getter.
  //
  // Check NEContainer.ne_coroutine.read
  // CHECK-LABEL: sil hidden [ossa] @$s15accessor_borrow11NEContainerV12ne_coroutineAA2NEVvr : $@yield_once @convention(method) (@guaranteed NEContainer) -> @lifetime(borrow 0) @yields @guaranteed NE {
  //
  // Do not synthesize NEContainer.ne_coroutine.getter
  // CHECK-NOT: $s15accessor_borrow11NEContainerV12ne_coroutineAA2NEVvg
  var ne_coroutine: NE {
    _read {
      yield NE()
    }
  }
}
