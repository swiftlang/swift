// RUN: %target-swift-frontend -emit-sil -parse-as-library -O -module-name=test \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature AddressableTypes \
// RUN:   %s | %FileCheck %s

// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_LifetimeDependence

// Enable this test as soon as CollectionOfOne is marked @_addressableForDependencies.
// REQUIRES: rdar145687827

// CHECK-LABEL: sil {{.*}}@$s4test0A10OneIntSpan1cs0D0VySiGs012CollectionOfB0VySiG_tF : $@convention(thin) (@in_guaranteed CollectionOfOne<Int>) -> @lifetime(borrow address_for_deps 0) @owned Span<Int> {
// CHECK: bb0(%0 : $*CollectionOfOne<Int>):
// CHECK:   [[RP:%.*]] = address_to_pointer {{.*}}%0 to $Builtin.RawPointer
// CHECK:   [[UP:%.*]] = struct $UnsafeRawPointer ([[RP]])
// CHECK:   [[OP:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt, [[UP]]
// CHECK:   [[SPAN:%.*]] = struct $Span<Int> ([[OP]]
// CHECK:   return [[SPAN]]
// CHECK-LABEL: } // end sil function '$s4test0A10OneIntSpan1cs0D0VySiGs012CollectionOfB0VySiG_tF'
@available(SwiftStdlib 6.2, *)
@lifetime(borrow c)
public func testOneIntSpan(c: CollectionOfOne<Int>) -> Span<Int> {
  c.span
}
