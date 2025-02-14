// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import Subscripts

func testGetX() -> CInt {
    let derivedDerived = CopyTrackedDerivedDerivedClass(42)
    return derivedDerived[0]
}

let _ = testGetX()

// CHECK: define {{.*}}linkonce_odr{{.*}} i32 @{{(.*)(30CopyTrackedDerivedDerivedClass39__synthesizedBaseCall_operatorSubscript|__synthesizedBaseCall_operatorSubscript@CopyTrackedDerivedDerivedClass)(.*)}}(ptr {{.*}} %[[THIS_PTR:.*]], i32 {{.*}})
// CHECK: %[[ADD_PTR:.*]] = getelementptr inbounds i8, ptr %{{.*}}, i{{32|64}} 4
// CHECK: {{.*}}call {{.*}} i32 @{{.*}}CopyTrackedBaseClass{{.*}}(ptr {{.*}} %[[ADD_PTR]], i32 {{.*}}){{.*}}
