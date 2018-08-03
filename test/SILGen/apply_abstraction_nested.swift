
// RUN: %target-swift-frontend -enable-sil-ownership -emit-silgen %s | %FileCheck %s

infix operator ~>

protocol P { }

func bar<T:P>(_: inout T) -> (()) -> () { return {_ in ()} }
func baz<T:P>(_: inout T) -> (Int) -> () { return {_ in ()} }

func ~> <T: P, Args, Result>(
  x: inout T,
  m: (_ x: inout T) -> ((Args) -> Result)
) -> ((Args) -> Result) {
  return m(&x)
}

struct X : P {}

var a = X()
(a~>bar)(())

// CHECK:  [[CHAINED_FUNC:%.*]] = apply {{%.*}}<X, (), ()>({{%.*}}, {{%.*}}) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : P> (@inout τ_0_0, @noescape @callee_guaranteed (@inout τ_0_0) -> @owned @callee_guaranteed (@in_guaranteed τ_0_1) -> @out τ_0_2) -> @owned @callee_guaranteed (@in_guaranteed τ_0_1) -> @out τ_0_2
// CHECK:  [[REABSTRACT:%.*]] = function_ref @$SytytIegnr_Ieg_TR
// CHECK:  [[CHAINED_FUNC_REABSTRACTED:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[CHAINED_FUNC]])
// CHECK:  [[BORROW:%.*]] = begin_borrow [[CHAINED_FUNC_REABSTRACTED]]
// CHECK:  apply [[BORROW]]() : $@callee_guaranteed () -> ()
