// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=swift-6 -enable-experimental-feature NoncopyableGenerics %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s
// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature NoncopyableGenerics %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

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

// CHECK: define {{.*}}linkonce_odr{{.*}} ptr @{{.*}}__synthesizedBaseCall___synthesizedBaseGetterAccessor{{.*}}

// CHECK: define {{.*}}linkonce_odr{{.*}} ptr @{{.*}}__synthesizedBaseCall___synthesizedBaseSetterAccessor{{.*}}

// CHECK: define {{.*}}linkonce_odr{{.*}} ptr @{{.*}}__synthesizedBaseGetterAccessor{{.*}}
// CHECK: %[[VPTR:.*]] = getelementptr inbounds %struct.NonCopyableHolder
// CHECK: ret ptr %[[VPTR]]

// CHECK: define {{.*}}linkonce_odr{{.*}} ptr @{{.*}}__synthesizedBaseSetterAccessor{{.*}}
// CHECK: %[[VPTRS:.*]] = getelementptr inbounds %struct.NonCopyableHolder
// CHECK: ret ptr %[[VPTRS]]
