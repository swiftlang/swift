// RUN: %target-swift-emit-ir %s -module-name main -enable-experimental-feature Embedded -wmo -parse-as-library | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

// swift_retain must be emitted with a body, not just declared.
// CHECK: define {{.*}}@swift_retain(
// CHECK-NOT: {{^}}declare {{.*}}@swift_retain(

public class C {
  var x: Int = 0
}

public func make() -> C { return C() }
