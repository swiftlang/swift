// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=default %s -validate-tbd-against-ir=none | %FileCheck %s

import Fields

func testGetX() -> CInt {
    let derivedDerived = CopyTrackedDerivedDerivedClass(42)
    return derivedDerived.x
}

let _ = testGetX()

// CHECK: define {{.*}}linkonce_odr{{.*}} i32 @{{(.*)(30CopyTrackedDerivedDerivedClass33__synthesizedBaseGetterAccessor_x|__synthesizedBaseGetterAccessor_x@CopyTrackedDerivedDerivedClass)(.*)}}(ptr {{.*}} %[[THIS_PTR:.*]])
// CHECK: %[[ADD_PTR:.*]] = getelementptr inbounds i8, ptr %{{.*}}, i{{32|64}} 4
// CHECK: %[[X:.*]] = getelementptr inbounds{{.*}} %class.CopyTrackedBaseClass, ptr %[[ADD_PTR]], i32 0, i32 0

func testSetX(_ derived: inout CopyTrackedDerivedClass) {
    derived.x = 42
}

// The synthesized cast to the base class can't throw, so calling it doesn't
// need an exception landing pad.
// CHECK-LABEL: define {{.*}} @{{.*}}testSetX
// CHECK-NOT: invoke
// CHECK: call {{.*}}__swift_interopStaticCast_{{.*}}CopyTrackedDerivedClass{{.*}}CopyTrackedBaseClass
// CHECK-NOT: invoke
// CHECK: ret void
