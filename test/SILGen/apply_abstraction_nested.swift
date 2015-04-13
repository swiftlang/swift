// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

infix operator ~> { precedence 255 associativity left }

protocol P { }

func bar<T:P>(inout _: T)() {}
func baz<T:P>(inout _: T)(_:Int) {}

func ~> <T: P, Args, Result>(
  inout x: T,
  m: (inout x: T)->((Args)->Result)
) -> (Args->Result) {
  return m(x: &x)
}

struct X : P {}

var a = X()
(a~>bar)()

// CHECK:  [[CHAINED_FUNC:%.*]] = apply {{%.*}}<X, (), ()>({{%.*}}, {{%.*}}) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : P> (@inout τ_0_0, @owned @callee_owned (@inout τ_0_0) -> @owned @callee_owned (@out τ_0_2, @in τ_0_1) -> ()) -> @owned @callee_owned (@out τ_0_2, @in τ_0_1) -> ()
// CHECK:  [[REABSTRACT:%.*]] = function_ref @_TTRXFo_iT__iT__XFo__dT__
// CHECK:  [[CHAINED_FUNC_REABSTRACTED:%.*]] = partial_apply [[REABSTRACT]]([[CHAINED_FUNC]])
// CHECK:  apply [[CHAINED_FUNC_REABSTRACTED]]() : $@callee_owned () -> ()
