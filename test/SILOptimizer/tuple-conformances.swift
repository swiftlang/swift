// RUN: %target-swift-frontend -emit-sil %s -enable-experimental-feature TupleConformances | %FileCheck %s

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

@_transparent public func transparentCaller<each T: P>(tuple: (repeat each T).Type) {
  callee(t: tuple)
  transparentCallee(t: tuple)
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

// FIXME: These should call Int.protocolMethod()!

// CHECK: [[FN:%.*]] = function_ref @$sBT4mainRvzAA1PRzlE14protocolMethodyyFZ : $@convention(method) <each τ_0_0 where repeat each τ_0_0 : P> (@thin (repeat each τ_0_0).Type) -> ()
// CHECK: apply [[FN]]<Pack{Int}>(%4) : $@convention(method) <each τ_0_0 where repeat each τ_0_0 : P> (@thin (repeat each τ_0_0).Type) -> ()

// CHECK: [[FN:%.*]] = function_ref @$sBT4mainRvzAA1PRzlE14protocolMethodyyFZ : $@convention(method) <each τ_0_0 where repeat each τ_0_0 : P> (@thin (repeat each τ_0_0).Type) -> ()
// CHECK: apply [[FN]]<Pack{Int}>(%0) : $@convention(method) <each τ_0_0 where repeat each τ_0_0 : P> (@thin (repeat each τ_0_0).Type) -> ()
