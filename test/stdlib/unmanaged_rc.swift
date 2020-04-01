// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-sil -O | %FileCheck -check-prefix=OPT %s

// Make sure that when we invoke Unmanaged._withUnsafeGuaranteedRef, we do not
// have any ref count overhead.
public class Klass {}
public class KlassContainer {
    let k = Klass()
}

@inline(never)
public func myPrint(_ k: Klass) { print(k) }

// Check the codegen of _withUnsafeGuaranteedRef
//
// CHECK-LABEL: sil public_external [transparent] [serialized] @$ss9UnmanagedV24_withUnsafeGuaranteedRefyqd__qd__xKXEKlF : $@convention(method) <Instance where Instance : AnyObject><Result> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject> (@guaranteed τ_0_0) -> (@out τ_0_1, @error Error) for <Instance, Result>, Unmanaged<Instance>) -> (@out Result, @error Error) {
// CHECK: bb0([[RESULT:%.*]] : $*Result, [[FUNC:%.*]] : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject> (@guaranteed τ_0_0) -> (@out τ_0_1, @error Error) for <Instance, Result>, [[UNMANAGED:%.*]] : $Unmanaged<Instance>):
// CHECK: [[UNMANAGED_REF:%.*]] = struct_extract [[UNMANAGED]]
// CHECK: [[REF:%.*]] = unmanaged_to_ref [[UNMANAGED_REF]]
// CHECK: [[REF_MARK_DEP:%.*]] = mark_dependence [[REF]]
// CHECK: try_apply {{%.*}}([[RESULT]], [[REF_MARK_DEP]]) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : _RefCountedObject> (@guaranteed τ_0_0) -> (@out τ_0_1, @error Error) for <Instance, Result>, normal bb2, error bb1
// CHECK-NOT: destroy_value
// CHECK: } // end sil function '$ss9UnmanagedV24_withUnsafeGuaranteedRefyqd__qd__xKXEKlF'

// OPT-LABEL: sil @$s12unmanaged_rc12useUnmanagedyys0D0VyAA14KlassContainerCGF : $@convention(thin) (Unmanaged<KlassContainer>) -> () {
// OPT: bb0([[UNMANAGED:%.*]] :
// OPT:   [[UNMANAGED_REF:%.*]] = struct_extract [[UNMANAGED]]
// OPT:   [[REF:%.*]] = unmanaged_to_ref [[UNMANAGED_REF]]
// OPT:   [[REF_ELT_ADDR:%.*]] = ref_element_addr [[REF]] : $KlassContainer, #KlassContainer.k
// OPT:   [[VALUE:%.*]] = load [[REF_ELT_ADDR]]
// OPT:   apply {{%.*}}([[VALUE]])
// OPT: } // end sil function '$s12unmanaged_rc12useUnmanagedyys0D0VyAA14KlassContainerCGF'
public func useUnmanaged(_ u: Unmanaged<KlassContainer>) {
    u._withUnsafeGuaranteedRef {
        myPrint($0.k)
    }
}
