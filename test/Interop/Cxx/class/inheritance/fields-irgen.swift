// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import Fields

func testGetX() -> CInt {
    let derivedDerived = CopyTrackedDerivedDerivedClass(42)
    return derivedDerived.x
}

let _ = testGetX()

// CHECK: define {{.*}}linkonce_odr{{.*}} i32 @{{.*}}__synthesizedBaseCall___synthesizedBaseGetterAccessor{{.*}}(ptr {{.*}} %[[THIS_PTR:.*]])
// CHECK: %[[ADD_PTR:.*]] = getelementptr inbounds i8, ptr %{{.*}}, i64 4
// CHECK: call noundef i32 @{{.*}}__synthesizedBaseGetterAccessor{{.*}}(ptr {{.*}} %[[ADD_PTR]])
