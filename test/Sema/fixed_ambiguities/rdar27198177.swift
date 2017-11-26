// RUN: %target-swift-frontend -emit-sil -verify %s -swift-version 4 | %FileCheck %s

let arr = ["A", "B", "C"]
let lazy: LazyMapCollection = arr.lazy.map { $0 }
// CHECK: function_ref @_T0s22LazyCollectionProtocolPsE6filters0a6FilterB0Vy8ElementsQzGSb7ElementQzcF : $@convention(method) <τ_0_0 where τ_0_0 : LazyCollectionProtocol> (@owned @callee_guaranteed (@in τ_0_0.Element) -> Bool, @in_guaranteed τ_0_0) -> @out LazyFilterCollection<τ_0_0.Elements>
_ = lazy.filter { $0 > "A" }.count
