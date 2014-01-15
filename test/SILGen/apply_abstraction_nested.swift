// RUN: %swift -emit-silgen %s | FileCheck %s
operator infix ~> { precedence 255 associativity left }

protocol P { }

func bar<T:P>(_: @inout T)() {}
func baz<T:P>(_: @inout T)(_:Int) {}

@assignment
func ~> <T: P, Args, Result>(
  x: @inout T,
  m: (x: @inout T)->((Args)->Result)
) -> (Args->Result) {
  return m(&x)
}

struct X : P {}

var a = X()
(a~>bar)()

// CHECK:  [[CHAINED_FUNC:%.*]] = apply {{%.*}}<T = X, Args = (), Result = ()>({{%.*}}, {{%.*}}) : $@thin <$T_0_0, $T_0_1, $T_0_2 where $T_0_0 : P> (@inout $T_0_0, @owned @callee_owned (@inout $T_0_0) -> @owned @callee_owned (@out $T_0_2, @in $T_0_1) -> ()) -> @owned @callee_owned (@out $T_0_2, @in $T_0_1) -> ()
// CHECK:  [[REABSTRACT:%.*]] = function_ref @_TTRXFo_iT__iT__XFo__dT__
// CHECK:  [[CHAINED_FUNC_REABSTRACTED:%.*]] = partial_apply [[REABSTRACT]]([[CHAINED_FUNC]])
// CHECK:  apply [[CHAINED_FUNC_REABSTRACTED]]() : $@callee_owned () -> ()
