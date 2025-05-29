
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name vtable_thunks_reabstraction_final %s | %FileCheck %s

protocol Fooable {
  func foo(_ x: Int) -> Int?
}

protocol Barrable {
  associatedtype Bar
  func foo(_ x: Bar) -> Bar?
}

class GenericSuper<T> {
  func foo(_ x: T) -> T? {
    return nil
  }
}

class NongenericSub: GenericSuper<Int>, Fooable {
  override func foo(_ x: Int) -> Int? {
    return 6502
  }
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s33vtable_thunks_reabstraction_final13NongenericSubCAA7FooableA2aDP3foo{{[_0-9a-zA-Z]*}}FTW
// CHECK:         class_method {{%.*}} : $NongenericSub, #NongenericSub.foo : {{.*}}

class GenericSub<U: AnyObject>: GenericSuper<U>, Barrable {
  override func foo(_ x: U) -> U? {
    return x
  }
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s33vtable_thunks_reabstraction_final10GenericSubCyxGAA8BarrableA2aEP3fooy3BarQzSgAIFTW
// CHECK:         class_method {{%.*}} : $GenericSub<τ_0_0>, #GenericSub.foo : {{.*}}, $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@in_guaranteed τ_0_0, @guaranteed GenericSub<τ_0_0>) -> @out Optional<τ_0_0>

class C {}

// CHECK-LABEL: sil hidden [ossa] @$s33vtable_thunks_reabstraction_final4testyyF
func test() {
  // CHECK: class_method {{%.*}} : $NongenericSub, #NongenericSub.foo : {{.*}}
  NongenericSub().foo(0)

  // FIXME: rdar://problem/21167978
  // let f = NongenericSub().curried(0)

  // CHECK:         class_method {{%.*}} : $GenericSub<C>, #GenericSub.foo : {{.*}}, $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@in_guaranteed τ_0_0, @guaranteed GenericSub<τ_0_0>) -> @out Optional<τ_0_0>
  GenericSub<C>().foo(C())
}
