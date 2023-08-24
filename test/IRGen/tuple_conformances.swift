// RUN: %target-swift-frontend -emit-ir -parse-stdlib -primary-file %s -enable-experimental-feature TupleConformances -parse-as-library | %FileCheck %s

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts

import Swift

protocol P {
  func f()
}

extension P {
  func f() {}
}

extension Builtin.TheTupleType: P where repeat each Elements: P {
}

func takesP<T: P>(_ t: T) {
  t.f()
}

struct S: P {}

func use() {
  takesP((S(), S()))
}

// CHECK-LABEL: define internal swiftcc void @"$sxxQp_t18tuple_conformances1PA2aBP1fyyFTW"({{i32|i64}} %0, ptr %"{{.*}}", ptr {{.*}} swiftself %1, ptr %Self, ptr %SelfWitnessTable) {{.*}} {

// CHECK-LABEL: define hidden swiftcc void @"$s18tuple_conformances3useyyF"() {{.*}} {
// CHECK: call ptr @"$s18tuple_conformances1SV_ACtxxQp_tAA1PAAWl"()
// CHECK: ret void

// CHECK-LABEL: define linkonce_odr hidden ptr @"$s18tuple_conformances1SV_ACtxxQp_tAA1PAAWl"() {{.*}} {


