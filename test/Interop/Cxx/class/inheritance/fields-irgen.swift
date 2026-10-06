// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s
// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none | %FileCheck %s --check-prefix=CAST

import Fields

func testGetX() -> CInt {
    let derivedDerived = CopyTrackedDerivedDerivedClass(42)
    return derivedDerived.x
}

let _ = testGetX()

func testSetX(_ derived: inout CopyTrackedDerivedClass) {
    derived.x = 42
}

// The synthesized cast to the base class can't throw, so calling it doesn't
// need an exception landing pad.
// CAST-LABEL: define {{.*}} @{{.*}}testSetX
// CAST-NOT: invoke
// CAST: call {{.*}}__swift_interopStaticCast_{{.*}}CopyTrackedDerivedClass{{.*}}CopyTrackedBaseClass
// CAST-NOT: invoke
// CAST: ret void

// CHECK: define {{.*}}linkonce_odr{{.*}} i32 @{{(.*)(30CopyTrackedDerivedDerivedClass33__synthesizedBaseGetterAccessor_x|__synthesizedBaseGetterAccessor_x@CopyTrackedDerivedDerivedClass)(.*)}}(ptr {{.*}} %[[THIS_PTR:.*]])
// CHECK: %[[ADD_PTR:.*]] = getelementptr inbounds i8, ptr %{{.*}}, i{{32|64}} 4
// CHECK: %[[X:.*]] = getelementptr inbounds{{.*}} %class.CopyTrackedBaseClass, ptr %[[ADD_PTR]], i32 0, i32 0
