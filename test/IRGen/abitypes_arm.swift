// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name abitypes -I %S/Inputs/abi %s -emit-ir | %FileCheck -check-prefix=%target-cpu-%target-os-abi -check-prefix=%target-cpu %s
// REQUIRES: CPU=armv7 || CPU=armv7k || CPU=armv7s

import c_gadget

class Foo {
  // Test that the makeOne() that we generate somewhere below doesn't
  // use arm_aapcscc for armv7.
  func callInline() -> Float {
    return makeOne(3,5).second
  }
}

// armv7: define internal void @makeOne(ptr noalias sret({{.*}}) align 4 %agg.result, float{{( noundef)?}} %f, float{{( noundef)?}} %s)
// armv7s: define internal void @makeOne(ptr noalias sret({{.*}}) align 4 %agg.result, float %f, float %s)
// armv7k: define internal %struct.One @makeOne(float {{.*}}%f, float {{.*}}%s)
