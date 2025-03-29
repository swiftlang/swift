// RUN: %target-swift-frontend -emit-sil %s -enable-experimental-feature TupleConformances | %FileCheck %s

// REQUIRES: swift_feature_TupleConformances

public typealias Tuple<each T> = (repeat each T)

public protocol P {
  static func protocolMethod()
}

extension Tuple: P where repeat each T: P {
  public static func protocolMethod() {}
}

extension Int: P {
  public static func protocolMethod() {}
}

public func callee<T: P>(t: T.Type) {
}

@_transparent
public func transparentCallee<T: P>(t: T.Type) {
  t.protocolMethod()
}

// Make sure we don't devirtualize the call when we inline transparentCallee()
// into transparentCaller(), because we might unwrap the tuple conformance
// later.

// CHECK-LABEL: sil [transparent] @$s4main17transparentCaller5tupleyxxQp_tm_tRvzAA1PRzlF : $@convention(thin) <each T where repeat each T : P> (@thin (repeat each T).Type) -> () {
@_transparent public func transparentCaller<each T: P>(tuple: (repeat each T).Type) {
  callee(t: tuple)

// CHECK: [[FN:%.*]] = witness_method $(repeat each T), #P.protocolMethod : <Self where Self : P> (Self.Type) -> () -> () : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@thick τ_0_0.Type) -> ()
// CHECK: apply [[FN:%.*]]<(repeat each T)>({{.*}})

  transparentCallee(t: tuple)

// FIXME: This one is wrong in the AST
  tuple.protocolMethod()
}

// Inlining transparentCaller() into caller() exercises the code path to unwrap
// a one-element tuple conformance.

public func caller() {
  transparentCaller(tuple: Int.self)
}

// CHECK-LABEL: sil @$s4main6calleryyF : $@convention(thin) () -> () {
// CHECK: [[FN:%.*]] = function_ref @$s4main6callee1tyxm_tAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@thick τ_0_0.Type) -> ()
// CHECK: apply [[FN]]<Int>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@thick τ_0_0.Type) -> ()

// Make sure the witness method call in transparentCallee() was devirtualized to Int.protocolMethod().

// CHECK: [[FN:%.*]] = function_ref @$sSi4mainE14protocolMethodyyFZ : $@convention(method) (@thin Int.Type) -> () // user: %6
// CHECK: apply [[FN]]({{.*}}) : $@convention(method) (@thin Int.Type) -> ()

// FIXME: This is the `tuple.protocolMethod()` in transparentCaller(). It should
// also refer to Int.protocolMethod()!

// CHECK: [[FN:%.*]] = function_ref @$sBT4mainRvzAA1PRzlE14protocolMethodyyFZ : $@convention(method) <each τ_0_0 where repeat each τ_0_0 : P> (@thin (repeat each τ_0_0).Type) -> ()
// CHECK: apply [[FN]]<Pack{Int}>({{.*}}) : $@convention(method) <each τ_0_0 where repeat each τ_0_0 : P> (@thin (repeat each τ_0_0).Type) -> ()
