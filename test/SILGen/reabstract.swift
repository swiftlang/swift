// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func takeFn<T>(f : T -> T?) {}
func liftOptional(x : Int) -> Int? { return x }

func test0() {
  takeFn(liftOptional)
}
// CHECK:    sil hidden @_TF10reabstract5test0FT_T_ : $@thin () -> () {
// CHECK:      [[T0:%.*]] = function_ref @_TF10reabstract6takeFn
//   Emit a generalized reference to liftOptional.
//   TODO: just emit a globalized thunk
// CHECK-NEXT: reabstract.liftOptional
// CHECK-NEXT: [[T1:%.*]] = function_ref @_TF10reabstract12liftOptional
// CHECK-NEXT: [[T2:%.*]] = thin_to_thick_function [[T1]]
// CHECK-NEXT: reabstraction thunk
// CHECK-NEXT: [[T3:%.*]] = function_ref [[THUNK:@.*]] :
// CHECK-NEXT: [[T4:%.*]] = partial_apply [[T3]]([[T2]])
// CHECK-NEXT: apply [[T0]]<Int>([[T4]])
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// CHECK:    sil shared [transparent] [thunk] [[THUNK]] : $@thin (@out Optional<Int>, @in Int, @owned @callee_owned (Int) -> Optional<Int>) -> () {
// CHECK:      [[T0:%.*]] = load %1 : $*Int
// CHECK-NEXT: [[T1:%.*]] = apply %2([[T0]])
// CHECK-NEXT: store [[T1]] to %0
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return
