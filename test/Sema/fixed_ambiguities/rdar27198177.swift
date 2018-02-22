// RUN: %target-swift-frontend -emit-sil -verify %s -swift-version 4 | %FileCheck %s

let arr = ["A", "B", "C"]
let lazy: LazyMapCollection = arr.lazy.map { $0 }
// CHECK: function_ref @$Ss17LazyMapCollectionV6filterys0a7CompactbC0Vyxq_GSbq_cF : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Collection> (@owned @callee_guaranteed (@in τ_0_1) -> Bool, @in_guaranteed LazyMapCollection<τ_0_0, τ_0_1>) -> @out LazyCompactMapCollection<τ_0_0, τ_0_1>
_ = lazy.filter { $0 > "A" }.count
