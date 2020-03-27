// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

public protocol P {}

public struct S : P {}

public protocol Curryable {
  func concreteRequirement()
  func genericRequirement<T : P>(_: T)
  func concreteRequirementSelf() -> Self
  func genericRequirementSelf<T : P>(_: T) -> Self
  func concreteRequirementInt() -> Int
  func genericRequirementInt<T : P>(_: T) -> Int
}

public protocol Q {
  associatedtype S : Curryable
  associatedtype U : P
}

@_optimize(none)
public func sinkThrows(_: () throws -> ()) {}

@_optimize(none)
public func sink0<Result>(_: () -> Result) {}

@_optimize(none)
public func sink1<Input, Result>(_: Input.Type, _: (Input) -> Result) {}

public func abstractCurried<T : Q>(t: T, s: T.S, u: T.U.Type) {
  sink0(s.concreteRequirement)
  sink1(u, s.genericRequirement)
  sink0(s.concreteRequirementSelf)
  sink1(u, s.genericRequirementSelf)
  sink0(s.concreteRequirementInt)
  sink1(u, s.genericRequirementInt)
  sinkThrows({ () throws -> () in s.concreteRequirement() })
}

// CHECK-LABEL: sil private @$s23sil_combine_curry_thunk15abstractCurried1t1s1uyx_1SQz1UQzmtAA1QRzlFyycAGcfu_ : $@convention(thin) <T where T : Q> (@in_guaranteed T.S) -> @owned @callee_guaranteed () -> () {
// CHECK: [[ARG:%.*]] = alloc_stack $T.S
// CHECK: copy_addr %0 to [initialization] [[ARG]] : $*T.S
// CHECK: [[FN:%.*]] = witness_method $T.S, #Curryable.concreteRequirement : <Self where Self : Curryable> (Self) -> () -> () : $@convention(witness_method: Curryable) <τ_0_0 where τ_0_0 : Curryable> (@in_guaranteed τ_0_0) -> ()
// CHECK: partial_apply [callee_guaranteed] [[FN]]<T.S>([[ARG]])
// CHECK: return

// CHECK-LABEL: sil private @$s23sil_combine_curry_thunk15abstractCurried1t1s1uyx_1SQz1UQzmtAA1QRzlFyAIcAGcfu1_ : $@convention(thin) <T where T : Q> (@in_guaranteed T.S) -> @owned @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T.U> {
// CHECK: [[ARG:%.*]] = alloc_stack $T.S
// CHECK: copy_addr %0 to [initialization] [[ARG]] : $*T.S
// CHECK: [[FN:%.*]] = witness_method $T.S, #Curryable.genericRequirement : <Self where Self : Curryable><T where T : P> (Self) -> (T) -> () : $@convention(witness_method: Curryable) <τ_0_0 where τ_0_0 : Curryable><τ_1_0 where τ_1_0 : P> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> ()
// CHECK: partial_apply [callee_guaranteed] [[FN]]<T.S, T.U>([[ARG]])
// CHECK: return

// CHECK-LABEL: sil private @$s23sil_combine_curry_thunk15abstractCurried1t1s1uyx_1SQz1UQzmtAA1QRzlFAGycAGcfu3_ : $@convention(thin) <T where T : Q> (@in_guaranteed T.S) -> @owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T.S> {
// CHECK: [[ARG:%.*]] = alloc_stack $T.S
// CHECK: copy_addr %0 to [initialization] [[ARG]] : $*T.S
// CHECK: [[FN:%.*]] = witness_method $T.S, #Curryable.concreteRequirementSelf : <Self where Self : Curryable> (Self) -> () -> Self : $@convention(witness_method: Curryable) <τ_0_0 where τ_0_0 : Curryable> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK: partial_apply [callee_guaranteed] [[FN]]<T.S>([[ARG]])
// CHECK: return

// CHECK-LABEL: sil private @$s23sil_combine_curry_thunk15abstractCurried1t1s1uyx_1SQz1UQzmtAA1QRzlFAgIcAGcfu5_ : $@convention(thin) <T where T : Q> (@in_guaranteed T.S) -> @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T.U, T.S> {
// CHECK: [[ARG:%.*]] = alloc_stack $T.S
// CHECK: copy_addr %0 to [initialization] [[ARG]] : $*T.S
// CHECK: [[FN:%.*]] = witness_method $T.S, #Curryable.genericRequirementSelf : <Self where Self : Curryable><T where T : P> (Self) -> (T) -> Self : $@convention(witness_method: Curryable) <τ_0_0 where τ_0_0 : Curryable><τ_1_0 where τ_1_0 : P> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK: partial_apply [callee_guaranteed] [[FN]]<T.S, T.U>([[ARG]])
// CHECK: return

// CHECK-LABEL: sil private @$s23sil_combine_curry_thunk15abstractCurried1t1s1uyx_1SQz1UQzmtAA1QRzlFSiycAGcfu7_ : $@convention(thin) <T where T : Q> (@in_guaranteed T.S) -> @owned @callee_guaranteed () -> Int {
// CHECK: [[ARG:%.*]] = alloc_stack $T.S
// CHECK: copy_addr %0 to [initialization] [[ARG]] : $*T.S
// CHECK: [[FN:%.*]] = witness_method $T.S, #Curryable.concreteRequirementInt : <Self where Self : Curryable> (Self) -> () -> Int : $@convention(witness_method: Curryable) <τ_0_0 where τ_0_0 : Curryable> (@in_guaranteed τ_0_0) -> Int
// CHECK: partial_apply [callee_guaranteed] %4<T.S>(%2) : $@convention(witness_method: Curryable) <τ_0_0 where τ_0_0 : Curryable> (@in_guaranteed τ_0_0) -> Int
// CHECK: return

// CHECK-LABEL: sil private @$s23sil_combine_curry_thunk15abstractCurried1t1s1uyx_1SQz1UQzmtAA1QRzlFSiAIcAGcfu9_ : $@convention(thin) <T where T : Q> (@in_guaranteed T.S) -> @owned @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> Int for <T.U> {
// CHECK: [[ARG:%.*]] = alloc_stack $T.S
// CHECK: copy_addr %0 to [initialization] [[ARG]] : $*T.S
// CHECK: [[FN:%.*]] = witness_method $T.S, #Curryable.genericRequirementInt : <Self where Self : Curryable><T where T : P> (Self) -> (T) -> Int : $@convention(witness_method: Curryable) <τ_0_0 where τ_0_0 : Curryable><τ_1_0 where τ_1_0 : P> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> Int
// CHECK: partial_apply [callee_guaranteed] [[FN]]<T.S, T.U>([[ARG]])
// CHECK: return