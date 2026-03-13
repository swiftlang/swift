// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=swift-6 %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s
// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=upcoming-swift %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import MoveOnlyCxxValueType

func testGetX() -> CInt {
    let derived = NonCopyableHolderDerivedDerived(42)
    return derived.x.x
}

let _ = testGetX()

func testSetX(_ x: CInt) {
    var derived = NonCopyableHolderDerivedDerived(42)
    derived.x.x = 2
}

testSetX(2)

// CHECK: define {{.*}}linkonce_odr{{.*}} ptr @{{(.*)(31NonCopyableHolderDerivedDerived*33__synthesizedBaseGetterAccessor_x|__synthesizedBaseGetterAccessor_x@NonCopyableHolderDerivedDerived)(.*)}}
// CHECK: %[[VPTR:.*]] = getelementptr inbounds{{.*}} %struct.NonCopyableHolder
// CHECK: ret ptr %[[VPTR]]

// CHECK: define {{.*}}linkonce_odr{{.*}} ptr @{{(.*)(31NonCopyableHolderDerivedDerived33__synthesizedBaseSetterAccessor_x|__synthesizedBaseSetterAccessor_x@NonCopyableHolderDerivedDerived)(.*)}}
// CHECK: %[[VPTRS:.*]] = getelementptr inbounds{{.*}} %struct.NonCopyableHolder
// CHECK: ret ptr %[[VPTRS]]
