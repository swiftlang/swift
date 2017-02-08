// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

struct A {
        func g<U>(_ recur: (A, U) -> U) -> (A, U) -> U {
                return { _, x in return x }
        }
        // CHECK-LABEL: sil hidden @_T024call_chain_reabstraction1AV1f{{[_0-9a-zA-Z]*}}F
        // CHECK:         [[G:%.*]] = function_ref @_T024call_chain_reabstraction1AV1g{{[_0-9a-zA-Z]*}}F
        // CHECK:         [[G2:%.*]] = apply [[G]]<A>
        // CHECK:         [[REABSTRACT_THUNK:%.*]] = function_ref @_T024call_chain_reabstraction1AVAcCIxyir_AccCIxyyd_TR
        // CHECK:         [[REABSTRACT:%.*]] = partial_apply [[REABSTRACT_THUNK]]([[G2]])
        // CHECK:         apply [[REABSTRACT]]([[SELF:%.*]], [[SELF]])
        func f() {
                let recur: (A, A) -> A = { c, x in x }
                let b = g(recur)(self, self)
        }
}
