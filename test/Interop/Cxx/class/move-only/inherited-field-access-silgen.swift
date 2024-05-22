// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=swift-6 %s -validate-tbd-against-ir=none | %FileCheck %s
// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=upcoming-swift %s -validate-tbd-against-ir=none | %FileCheck %s

// REQUIRES: rdar128424443

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

// CHECK: sil shared [transparent] @$sSo024NonCopyableHolderDerivedD0V1xSo0aB0Vvlu : $@convention(method) (@in_guaranteed NonCopyableHolderDerivedDerived) -> UnsafePointer<NonCopyable>
// CHECK: {{.*}}(%[[SELF_VAL:.*]] : $*NonCopyableHolderDerivedDerived):
// CHECK: function_ref @{{.*}}__synthesizedBaseCall___synthesizedBaseGetterAccessor{{.*}} : $@convention(cxx_method) (@in_guaranteed NonCopyableHolderDerivedDerived) -> UnsafePointer<NonCopyable>
// CHECK-NEXT: apply %{{.*}}(%[[SELF_VAL]])

// CHECK: sil shared [transparent] @$sSo024NonCopyableHolderDerivedD0V1xSo0aB0Vvau : $@convention(method) (@inout NonCopyableHolderDerivedDerived) -> UnsafeMutablePointer<NonCopyable>
// CHECK: function_ref @{{.*}}__synthesizedBaseCall___synthesizedBaseSetterAccessor{{.*}} : $@convention(cxx_method) (@inout NonCopyableHolderDerivedDerived) -> UnsafeMutablePointer<NonCopyable>
