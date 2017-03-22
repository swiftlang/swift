// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

infix operator ~> { precedence 255 associativity left }

protocol P { }

func bar<T:P>(_: inout T) -> () -> () { return {_ in ()} }
func baz<T:P>(_: inout T) -> (Int) -> () { return {_ in ()} }

func ~> <T: P, Args, Result>(
  x: inout T,
  m: (_ x: inout T) -> ((Args) -> Result)
) -> ((Args) -> Result) {
  return m(&x)
}

struct X : P {}

var a = X()
(a~>bar)()

// CHECK:  [[CHAINED_FUNC:%.*]] = apply {{%.*}}<X, (), ()>({{%.*}}, {{%.*}}) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : P> (@inout τ_0_0, @owned @callee_owned (@inout τ_0_0) -> @owned @callee_owned (@in τ_0_1) -> @out τ_0_2) -> @owned @callee_owned (@in τ_0_1) -> @out τ_0_2
// CHECK:  [[REABSTRACT:%.*]] = function_ref @_T0ytytIxir_Ix_TR
// CHECK:  [[CHAINED_FUNC_REABSTRACTED:%.*]] = partial_apply [[REABSTRACT]]([[CHAINED_FUNC]])
// CHECK:  apply [[CHAINED_FUNC_REABSTRACTED]]() : $@callee_owned () -> ()
