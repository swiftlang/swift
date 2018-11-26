
// RUN: %target-swift-emit-silgen -module-name reabstract -Xllvm -sil-full-demangle -enable-sil-ownership %s | %FileCheck %s
// RUN: %target-swift-emit-sil -module-name reabstract -Xllvm -sil-full-demangle -enable-sil-ownership %s | %FileCheck %s --check-prefix=MANDATORY

func takeFn<T>(_ f : (T) -> T?) {}
func liftOptional(_ x : Int) -> Int? { return x }

func test0() {
  takeFn(liftOptional)
}
// CHECK:    sil hidden @$s10reabstract5test0yyF : $@convention(thin) () -> () {
//   Emit a generalized reference to liftOptional.
//   TODO: just emit a globalized thunk
// CHECK:      reabstract.liftOptional
// CHECK-NEXT: [[T1:%.*]] = function_ref @$s10reabstract12liftOptional{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: [[T2:%.*]] = thin_to_thick_function [[T1]]
// CHECK-NEXT: [[CVT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[T2]]
// CHECK-NEXT: reabstraction thunk
// CHECK-NEXT: [[T3:%.*]] = function_ref [[THUNK:@.*]] :
// CHECK-NEXT: [[T4:%.*]] = partial_apply [callee_guaranteed] [[T3]]([[CVT]])
// CHECK-NEXT: [[CVT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[T4]]
// CHECK: destroy_value [[T4]]
// CHECK-NEXT: destroy_value [[T2]]
// CHECK:      [[T0:%.*]] = function_ref @$s10reabstract6takeFn{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: apply [[T0]]<Int>([[CVT]])
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return
// CHECK-NEXT: } // end sil function '$s10reabstract5test0yyF'

// MANDATORY:    sil hidden @$s10reabstract5test0yyF : $@convention(thin) () -> () {
//   Emit a generalized reference to liftOptional.
//   TODO: just emit a globalized thunk
// MANDATORY:      reabstract.liftOptional
// MANDATORY-NEXT: [[T1:%.*]] = function_ref @$s10reabstract12liftOptional{{[_0-9a-zA-Z]*}}F
// MANDATORY-NEXT: [[T2:%.*]] = thin_to_thick_function [[T1]]
// MANDATORY-NEXT: strong_retain [[T2]]
// MANDATORY-NEXT: [[CVT:%.*]] = convert_escape_to_noescape [[T2]]
// MANDATORY-NEXT: //{{.*}}reabstraction thunk
// MANDATORY-NEXT: [[T3:%.*]] = function_ref [[THUNK:@.*]] :
// MANDATORY-NEXT: [[T4:%.*]] = partial_apply [callee_guaranteed] [[T3]]([[CVT]])
// MANDATORY-NEXT: [[CVT:%.*]] = convert_escape_to_noescape [[T4]]
// MANDATORY-NEXT: strong_release [[T2]]
// MANDATORY-NEXT: // function_ref
// MANDATORY-NEXT: [[T0:%.*]] = function_ref @$s10reabstract6takeFn{{[_0-9a-zA-Z]*}}F
// MANDATORY-NEXT: apply [[T0]]<Int>([[CVT]])
// MANDATORY-NEXT: strong_release [[T4]]
// MANDATORY-NEXT: strong_release [[T2]]
// MANDATORY-NEXT: tuple ()
// MANDATORY-NEXT: return
// MANDATORY-NEXT: } // end sil function '$s10reabstract5test0yyF'

// CHECK:    sil shared [transparent] [serializable] [reabstraction_thunk] [[THUNK]] : $@convention(thin) (@in_guaranteed Int, @noescape @callee_guaranteed (Int) -> Optional<Int>) -> @out Optional<Int> {
// CHECK:      [[T0:%.*]] = load [trivial] %1 : $*Int
// CHECK-NEXT: [[T1:%.*]] = apply %2([[T0]])
// CHECK-NEXT: store [[T1]] to [trivial] %0
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @$s10reabstract10testThrowsyyypF
// CHECK:         function_ref @$sytIegr_Ieg_TR
// CHECK:         function_ref @$syts5Error_pIegrzo_sAA_pIegzo_TR
func testThrows(_ x: Any) {
  _ = x as? () -> ()
  _ = x as? () throws -> ()
}

// Make sure that we preserve inout-ness when lowering types with maximum
// abstraction level -- <rdar://problem/21329377>
class C {}

struct Box<T> {
  let t: T
}

func notFun(_ c: inout C, i: Int) {}

func testInoutOpaque(_ c: C, i: Int) {
  var c = c
  let box = Box(t: notFun)
  box.t(&c, i)
}

// CHECK-LABEL: sil hidden @$s10reabstract15testInoutOpaque_1iyAA1CC_SitF
// CHECK:         function_ref @$s10reabstract6notFun_1iyAA1CCz_SitF
// CHECK:         thin_to_thick_function {{%[0-9]+}}
// CHECK:         function_ref @$s10reabstract1CCSiIegly_ACSiytIeglnr_TR
// CHECK:         partial_apply
// CHECK:         store
// CHECK:         load
// CHECK:         function_ref @$s10reabstract1CCSiytIeglnr_ACSiIegly_TR
// CHECK:         partial_apply
// CHECK:         apply
// CHECK: } // end sil function '$s10reabstract15testInoutOpaque_1iyAA1CC_SitF'

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s10reabstract1CCSiIegly_ACSiytIeglnr_TR : $@convention(thin) (@inout C, @in_guaranteed Int, @guaranteed @callee_guaranteed (@inout C, Int) -> ()) -> @out () {
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s10reabstract1CCSiytIeglnr_ACSiIegly_TR : $@convention(thin) (@inout C, Int, @guaranteed @callee_guaranteed (@inout C, @in_guaranteed Int) -> @out ()) -> () {

func closureTakingOptional(_ fn: (Int?) -> ()) {}
closureTakingOptional({ (_: Any) -> () in })

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypIgn_SiSgIegy_TR : $@convention(thin) (Optional<Int>, @noescape @callee_guaranteed (@in_guaranteed Any) -> ()) -> ()
// CHECK:   [[ANYADDR:%.*]] = alloc_stack $Any
// CHECK:   [[OPTADDR:%.*]] = init_existential_addr [[ANYADDR]] : $*Any, $Optional<Int>
// CHECK:   store %0 to [trivial] [[OPTADDR]] : $*Optional<Int>
// CHECK:   apply %1([[ANYADDR]]) : $@noescape @callee_guaranteed (@in_guaranteed Any) -> ()

// Same behavior as above with other ownership qualifiers.
func evenLessFun(_ s: __shared C, _ o: __owned C) {}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s10reabstract1CCACIeggx_A2CytIegnir_TR : $@convention(thin) (@in_guaranteed C, @in C, @guaranteed @callee_guaranteed (@guaranteed C, @owned C) -> ()) -> @out ()
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$s10reabstract1CCACytIegnir_A2CIeggx_TR : $@convention(thin) (@guaranteed C, @owned C, @guaranteed @callee_guaranteed (@in_guaranteed C, @in C) -> @out ()) -> ()
func testSharedOwnedOpaque(_ s: C, o: C) {
  let box = Box(t: evenLessFun)
  box.t(s, o)
}
