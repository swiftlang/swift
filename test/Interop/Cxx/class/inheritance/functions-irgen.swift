// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import Functions

func testGetX() -> CInt {
    let derivedDerived = CopyTrackedDerivedDerivedClass(42)
    return derivedDerived.getX()
}

let _ = testGetX()

// CHECK: define {{.*}} swiftcc i32 @"$sSo018CopyTrackedDerivedC5ClassV4getXs5Int32VyF"(ptr noalias swiftself dereferenceable(8) %[[SELF_PTR:.*]])
// CHECK: = call i32 @[[SYNTH_METHOD:.*]](ptr %[[SELF_PTR]])

// CHECK: define {{.*}}linkonce_odr{{.*}} i32 @[[SYNTH_METHOD]](ptr {{.*}} %[[THIS_PTR:.*]])
// CHECK: %[[ADD_PTR:.*]] = getelementptr inbounds i8, ptr %{{.*}}, i{{32|64}} 4
// CHECK: call{{.*}} i32 @{{.*}}(ptr {{.*}} %[[ADD_PTR]])
