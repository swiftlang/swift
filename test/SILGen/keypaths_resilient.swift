// RUN: %target-swift-emit-silgen %s -enable-library-evolution | %FileCheck %s

public enum E: Hashable {
  case e
}

public struct S {
  public var dict: [E: Int] = [:]
}

public func f() {
  let _ = \S.dict[.e]
}

// CHECK-LABEL: sil [ossa] @$s18keypaths_resilient1fyyF : $@convention(thin) () -> () {
// CHECK: [[ARG:%.*]] = alloc_stack $E
// CHECK: store %1 to [trivial] [[ARG]]
// CHECK: {{%.*}} = keypath $WritableKeyPath<S, Optional<Int>>, (root $S; stored_property #S.dict : $Dictionary<E, Int>; settable_property $Optional<Int>,  id @$sSDyq_Sgxcig : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@in_guaranteed τ_0_0, @guaranteed Dictionary<τ_0_0, τ_0_1>) -> @out Optional<τ_0_1>, getter @$sSDyq_SgxcipSDy18keypaths_resilient1EOSiGADSiTK : $@convention(keypath_accessor_getter) (@in_guaranteed Dictionary<E, Int>, @in_guaranteed E) -> @out Optional<Int>, setter @$sSDyq_SgxcipSDy18keypaths_resilient1EOSiGADSiTk : $@convention(keypath_accessor_setter) (@in_guaranteed Optional<Int>, @inout Dictionary<E, Int>, @in_guaranteed E) -> (), indices [%$0 : $E : $*E], indices_equals @$s18keypaths_resilient1EOTH : $@convention(keypath_accessor_equals) (@in_guaranteed E, @in_guaranteed E) -> Bool, indices_hash @$s18keypaths_resilient1EOTh : $@convention(keypath_accessor_hash) (@in_guaranteed E) -> Int, external #Dictionary.subscript<E, Int>) ([[ARG]])
// CHECK: return
