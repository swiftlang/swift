// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all -enable-sil-opaque-values -enable-experimental-feature AddressableParameters %s | %FileCheck %s

// REQUIRES: swift_feature_AddressableParameters

// Under opaque values an addressable parameter is a by-value object until
// AddressLowering. The address that the parameter promises must still be the
// caller's own, rather than a copy.

func takesAddressable(_ x: borrowing @_addressable UInt64) {}

// The caller passes the buffer that holds the value, not a second copy.
//
// CHECK-LABEL: sil hidden @$s{{.*}}callsWithAddressable{{.*}}F
// CHECK:         [[BUFFER:%.*]] = alloc_stack $UInt64
// CHECK-NOT:     alloc_stack
// CHECK:         apply {{%.*}}([[BUFFER]])
func callsWithAddressable() {
  let value: UInt64 = 0
  takesAddressable(value)
}

// A callee that forwards its own addressable parameter passes along the
// incoming address instead of copying it into a local buffer.
//
// CHECK-LABEL: sil hidden @$s{{.*}}forwardsAddressable{{.*}}F
// CHECK:       bb0([[ARG:%.*]] : @noImplicitCopy $*UInt64):
// CHECK-NOT:     alloc_stack
// CHECK:         apply {{%.*}}([[ARG]])
func forwardsAddressable(_ x: borrowing @_addressable UInt64) {
  takesAddressable(x)
}

// A trivial load only copies a value, so its source's address can't be passed
// to a call that runs after the access has ended. This interpolation loads the
// property inside a read access and calls 'appendInterpolation' after the access
// and the borrow of the class are over. The verifier rejects a call that uses
// the access's address at that point.
//
// CHECK-LABEL: sil hidden @$s{{.*}}interpolation{{.*}}F
final class Box {
  var i = 0
}
func interpolation(_ box: Box) -> String {
  return "\(box.i)"
}
