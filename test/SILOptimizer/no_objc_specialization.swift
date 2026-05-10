// RUN: %target-swift-frontend -import-objc-header %S/Inputs/NoObjCSpecialization.h -O -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

final class NoObjCSpecializationImpl<Value>: NSObject, NoObjCSpecialization {

    // CHECK-NOT: @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFToyt_Tg5 : $@convention(objc_method)
    // CHECK-LABEL: sil private [thunk] @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFTo : $@convention(objc_method)
    // CHECK: [[FUNC:%.*]] = function_ref @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFTf4nnd_n : $@convention(thin) <τ_0_0> (@in_guaranteed Any, Bool) -> @error any Error
    // CHECK: try_apply [[FUNC]]<Value>({{%.*}}, {{%.*}}) : $@convention(thin) <τ_0_0> (@in_guaranteed Any, Bool) -> @error any Error
    // CHECK-LABEL: sil shared @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFyt_Tg5Tf4nnd_n : $@convention(thin) (@in_guaranteed Any, Bool) -> @error any Error {
    // CHECK-LABEL: sil shared @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFTf4nnd_n : $@convention(thin) <Value> (@in_guaranteed Any, Bool) -> @error any Error {
    // CHECK: [[FUNC:%.*]] = function_ref @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFyt_Tg5Tf4nnd_n : $@convention(thin) (@in_guaranteed Any, Bool) -> @error any Error
    // CHECK: apply [nothrow] [[FUNC]]({{%.*}}, {{%.*}}) : $@convention(thin) (@in_guaranteed Any, Bool) -> @error any Error
    @_specialize(where Value == Void)
    func foo(withValue value: Any, andBool bool: Bool) throws {
        if Value.self == Void.self {
            print("Is Void with \(bool)")
        } else {
            print("Is \(value) with \(bool)")
        }
    }
}
