
// RUN: %target-swift-emit-ir %s -module-name main -parse-as-library -enable-experimental-feature Embedded -O -min-valid-pointer-value=0x200  | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

public func testit(_ s: S?) -> Bool {
  return s != nil
}

class C {}

public struct S {
  var a = InlineArray<57, UInt8>(repeating: 0)
  var b = C()
  var c = InlineArray<49, UInt8>(repeating: 0)
}

// CHECK-LABEL: define {{.*}} @"$e4main1SVSgWOg"(ptr %0)
// CHECK:         icmp {{.*}}, 511
// CHECK-LABEL: }
