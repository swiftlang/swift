// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import DerivedFieldGetterReturnsOwnedFRT

func testGetX() -> CInt {
    let derived = DerivedFieldFRT()
    return derived.value.testVal
}

let _ = testGetX()


// CHECK: define {{.*}}linkonce_odr{{.*}} ptr @{{.*}}__synthesizedBaseGetterAccessor_{{.*}}(ptr {{.*}} %[[THIS_PTR:.*]])
// CHECK: %[[VALUE_PTR_PTR:.*]] = getelementptr inbounds{{.*}} %class.BaseFieldFRT, ptr %{{.*}}, i32 0, i32 0
// CHECK: %[[VALUE_PTR:.*]] = load ptr, ptr %[[VALUE_PTR_PTR]]
// CHECK: call void @{{.*}}retainRefCounted{{.*}}(ptr noundef %[[VALUE_PTR]])
