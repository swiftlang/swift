// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import Subscripts

func testGetX() -> CInt {
    let derivedDerived = CopyTrackedDerivedDerivedClass(42)
    return derivedDerived[0]
}

let _ = testGetX()

// CHECK: define {{.*}}linkonce_odr{{.*}} i32 @{{.*}}__synthesizedBaseCall___synthesizedBaseCall_operatorSubscript{{.*}}(ptr {{.*}} %[[THIS_PTR:.*]], i32 {{.*}})
// CHECK: %[[ADD_PTR:.*]] = getelementptr inbounds i8, ptr %{{.*}}, i64 4
// CHECK: call noundef i32 @{{.*}}__synthesizedBaseCall_operatorSubscript{{.*}}(ptr {{.*}} %[[ADD_PTR]], i32 {{.*}})
