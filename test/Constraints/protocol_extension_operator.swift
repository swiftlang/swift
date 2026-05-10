// RUN: %target-swift-frontend -emit-sil %s -solver-enable-optimize-operator-defaults | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil %s -solver-disable-optimize-operator-defaults | %FileCheck %s

infix operator +++

protocol P {
  associatedtype A
  static func +++(_: Self, _: Self) -> Self
}

extension P {
  static func +++(_: Self, _: Self) -> Self { fatalError() }
}

extension P where A == Int {
  static func +++(_: Self, _: Self) -> Self { fatalError() }
}

struct G<T>: P {
  typealias A = T
}

// CHECK-LABEL: sil hidden @$s27protocol_extension_operator1PPAAE3pppoiyxx_xtFZ : $@convention(method) <Self where Self : P> (@in_guaranteed Self, @in_guaranteed Self, @thick Self.Type) -> @out Self {
// CHECK: function_ref @$s27protocol_extension_operator1PPAAE3pppoiyxx_xtFZ : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK: return
func testConcreteContext() {
  _ = G<Int>() +++ G<Int>()
}

// CHECK-LABEL: sil hidden @$s27protocol_extension_operator18testGenericContextyyxAA1PRzlF : $@convention(thin) <T where T : P> (@in_guaranteed T) -> () {
// CHECK: function_ref @$s27protocol_extension_operator1PPAAE3pppoiyxx_xtFZ : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK: return
func testGenericContext<T: P>(_: T) {
  _ = G<T>() +++ G<T>()
}

// CHECK-LABEL: sil hidden @$s27protocol_extension_operator22testConstrainedContextyyxAA1PRzSi1ARtzlF : $@convention(thin) <T where T : P, T.A == Int> (@in_guaranteed T) -> () {
// CHECK: function_ref @$s27protocol_extension_operator1PPAAE3pppoiyxx_xtFZ : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK: return
func testConstrainedContext<T: P>(_: T) where T.A == Int {
  _ = G<T>() +++ G<T>()
}
