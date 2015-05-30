// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Fooable {
  func foo(x: Int) -> Int?
}

protocol Barrable {
  typealias Bar
  func foo(x: Bar) -> Bar?
}

class GenericSuper<T> {
  func foo(x: T) -> T? {
    return nil
  }

  func curried(x: T)(y: T) -> T? {
    return nil
  }
}

class NongenericSub: GenericSuper<Int>, Fooable {
  override func foo(x: Int) -> Int? {
    return 6502
  }

  override func curried(x: Int)(y: Int) -> Int? {
    return 6502
  }
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC33vtable_thunks_reabstraction_final13NongenericSubS_7FooableS_FS1_3foouRq_S1__fq_FSiGSqSi_
// CHECK:         class_method {{%.*}} : $NongenericSub, #NongenericSub.foo!1 {{.*}}, $@convention(method) (@out Optional<Int>, @in Int, @guaranteed NongenericSub) -> ()

class GenericSub<U: AnyObject>: GenericSuper<U>, Barrable {
  override func foo(x: U) -> U? {
    return x
  }

  override func curried(x: U)(y: U) -> U? {
    return x
  }
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWuRq_Ss9AnyObject_GC33vtable_thunks_reabstraction_final10GenericSubq__S0_8BarrableS0_FS2_3foouRq_S2__fq_Fqq_S2_3BarGSqqq_S2_3Bar_
// CHECK:         class_method {{%.*}} : $GenericSub<U>, #GenericSub.foo!1 {{.*}}, $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@out Optional<τ_0_0>, @in τ_0_0, @guaranteed GenericSub<τ_0_0>) -> ()

class C {}

// CHECK-LABEL: sil hidden @_TF33vtable_thunks_reabstraction_final4testFT_T_
func test() {
  // CHECK: class_method {{%.*}} : $NongenericSub, #NongenericSub.foo!1 {{.*}}, $@convention(method) (@out Optional<Int>, @in Int, @guaranteed NongenericSub) -> ()
  NongenericSub().foo(0)

  // FIXME: rdar://problem/21167978
  // let f = NongenericSub().curried(0)

  // CHECK:         class_method {{%.*}} : $GenericSub<C>, #GenericSub.foo!1 {{.*}}, $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@out Optional<τ_0_0>, @in τ_0_0, @guaranteed GenericSub<τ_0_0>) -> ()
  GenericSub<C>().foo(C())
}
