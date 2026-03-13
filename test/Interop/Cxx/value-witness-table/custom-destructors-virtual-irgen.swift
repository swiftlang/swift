// RUN: %target-swift-frontend -cxx-interoperability-mode=default -I %S/Inputs %s -emit-ir | %FileCheck %s

import CustomDestructor

// CHECK-LABEL: define {{.*}}void @"$s4main022testHasVirtualBaseWithD10DestructoryySpys5Int32VGF"
// CHECK: call {{.*}}@{{_ZN28HasBaseWithVirtualDestructorD(1|2)Ev|"\?\?1HasBaseWithVirtualDestructor@@UEAA@XZ"}}(ptr
// CHECK: ret
public func testHasVirtualBaseWithVirtualDestructor(
  _ ptr: UnsafeMutablePointer<Int32>
) {
  _ = HasBaseWithVirtualDestructor(ptr)
}
