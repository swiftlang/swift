// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func f1(_: Any) {}
func f2(_: any BinaryInteger) {}
func f3(_: Int?) {}
func f4(_: AnyHashable) {}

func g<T>(_ t: T) -> T { return t }

// This behavior is a little strange, because it's better to bind T to the most
// specific subtype, which in this case is Int.

// CHECK-LABEL: sil hidden [ossa] @$s20binding_order_silgen4testyyF : $@convention(thin) () -> () {
func test() {
  // CHECK: [[FN:%.*]] = function_ref @$s20binding_order_silgen1gyxxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK-NEXT: apply [[FN]]<Any>({{.*}})
  f1(g(27))

  // CHECK: [[FN:%.*]] = function_ref @$s20binding_order_silgen1gyxxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK-NEXT: apply [[FN]]<any BinaryInteger>({{.*}})
  f2(g(27))

  // CHECK: [[FN:%.*]] = function_ref @$s20binding_order_silgen1gyxxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK-NEXT: apply [[FN]]<Optional<Int>>({{.*}})
  f3(g(27))

  // CHECK: [[FN:%.*]] = function_ref @$s20binding_order_silgen1gyxxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // apply [[FN]]<AnyHashable>({{.*}})
  f4(g(27))
}
