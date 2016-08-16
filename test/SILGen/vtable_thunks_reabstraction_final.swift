// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC33vtable_thunks_reabstraction_final13NongenericSubS_7FooableS_FS1_3foo
// CHECK:         class_method {{%.*}} : $NongenericSub, #NongenericSub.foo!1 {{.*}}, $@convention(method) (@in Int, @guaranteed NongenericSub) -> @out Optional<Int>

class GenericSub<U: AnyObject>: GenericSuper<U>, Barrable {
  override func foo(_ x: U) -> U? {
    return x
  }
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWuRxs9AnyObjectrGC33vtable_thunks_reabstraction_final10GenericSubx_S0_8BarrableS0_FS2_3foofwx3BarGSqwxS3__
// CHECK:         class_method {{%.*}} : $GenericSub<U>, #GenericSub.foo!1 {{.*}}, $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@in τ_0_0, @guaranteed GenericSub<τ_0_0>) -> @out Optional<τ_0_0>

class C {}

// CHECK-LABEL: sil hidden @_TF33vtable_thunks_reabstraction_final4testFT_T_
func test() {
  // CHECK: class_method {{%.*}} : $NongenericSub, #NongenericSub.foo!1 {{.*}}, $@convention(method) (@in Int, @guaranteed NongenericSub) -> @out Optional<Int>
  NongenericSub().foo(0)

  // FIXME: rdar://problem/21167978
  // let f = NongenericSub().curried(0)

  // CHECK:         class_method {{%.*}} : $GenericSub<C>, #GenericSub.foo!1 {{.*}}, $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@in τ_0_0, @guaranteed GenericSub<τ_0_0>) -> @out Optional<τ_0_0>
  GenericSub<C>().foo(C())
}
