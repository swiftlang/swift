// RUN: %swift -emit-silgen %s | FileCheck %s

func takeFn<T>(f : T -> T?) {}
func liftOptional(x : Int) -> Int? { return x }

func test0() {
  takeFn(liftOptional)
}
// CHECK:    sil @_T10reabstract5test0FT_T_ : $@thin () -> () {
// CHECK:      [[T0:%.*]] = function_ref @_T10reabstract6takeFnU__FT1fFQ_GSqQ___T_
//   Emit a generalized reference to liftOptional.
//   TODO: just emit a globalized thunk
// CHECK-NEXT: reabstract.liftOptional
// CHECK-NEXT: [[T1:%.*]] = function_ref @_T10reabstract12liftOptionalFT1xSi_GSqSi_
// CHECK-NEXT: [[T2:%.*]] = thin_to_thick_function [[T1]]
// CHECK-NEXT: thunk
// CHECK-NEXT: [[T3:%.*]] = function_ref [[THUNK:@.*]] :
// CHECK-NEXT: [[T4:%.*]] = partial_apply [[T3]]([[T2]])
// CHECK-NEXT: apply [[T0]]<T = Int>([[T4]])
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// CHECK:    sil internal [transparent] [[THUNK]] : $@thin (@out Optional<Int64>, @in Int64, @owned @callee_owned (Int64) -> Optional<Int64>) -> () {
// CHECK:      [[T0:%.*]] = load %1 : $*Int64
// CHECK-NEXT: [[T1:%.*]] = apply %2([[T0]])
// CHECK-NEXT: store [[T1]] to %0
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return
