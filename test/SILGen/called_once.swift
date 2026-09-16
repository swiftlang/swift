// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

func makeClosure() -> @called(once) () -> Void {
  return {}
}

// CHECK-LABEL: sil hidden [ossa] @$s11called_once12testCallOnceyyyyXEnF : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> () {
// CHECK: bb0([[F:%.*]] : @owned $@noescape @called(once) @callee_owned () -> ()):
// CHECK:  [[LOCAL:%.*]] = alloc_box ${ let @called(once) @callee_owned () -> () }, let, name "local"
// CHECK:  [[CLOSURE_REF:%.*]] = function_ref @$s11called_once12testCallOnceyyyyXEnFyyXOfU_ : $@convention(thin) () -> ()
// CHECK:  [[CLOSURE_CALLED_ONCE:%.*]] = convert_function [[CLOSURE_REF]] : $@convention(thin) () -> () to $@convention(thin) @called(once) () -> ()
// CHECK: } // end sil function '$s11called_once12testCallOnceyyyyXEnF'
func testCallOnce(_ f: @called(once) () -> Void) {
  let local: @called(once) () -> Void = {}
  _ = local
}

func run() {
  // CHECK-LABEL: sil private [ossa] @$s11called_once3runyyFyyXEfU_ : $@convention(thin) () -> ()
  testCallOnce { }
}

// `@escaping` `@called(once)` parameters keep `@callee_owned` (no `@noescape`)
// throughout - contrast with `testCallOnce` above, where `f` is implicitly
// `@noescape` and every occurrence of its type carries that annotation.
// CHECK-LABEL: sil hidden [ossa] @$s11called_once20testCallOnceEscapingyyyyXOnF : $@convention(thin) (@owned @called(once) @callee_owned () -> ()) -> () {
// CHECK: bb0([[F:%.*]] : @owned $@called(once) @callee_owned () -> ()):
// CHECK:  [[LOCAL:%.*]] = alloc_box ${ let @called(once) @callee_owned () -> () }, let, name "local"
// CHECK:  [[CLOSURE_REF:%.*]] = function_ref @$s11called_once20testCallOnceEscapingyyyyXOnFyyXOfU_ : $@convention(thin) () -> ()
// CHECK:  [[CLOSURE_CALLED_ONCE:%.*]] = convert_function [[CLOSURE_REF]] : $@convention(thin) () -> () to $@convention(thin) @called(once) () -> ()
// CHECK: } // end sil function '$s11called_once20testCallOnceEscapingyyyyXOnF'
func testCallOnceEscaping(_ f: @escaping @called(once) () -> Void) {
  let local: @called(once) () -> Void = {}
  _ = local
}

// CHECK-LABEL: sil hidden [ossa] @$s11called_once22testClosureWithCapture1fyyyXEn_tF : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> () {
// CHECK:  [[FN:%.*]] = begin_borrow [lexical] [var_decl] %1 : ${ var @noescape @called(once) @callee_owned () -> () }
// CHECK:  [[FN_PROJ:%.*]] = project_box [[FN]] : ${ var @noescape @called(once) @callee_owned () -> () }
// CHECK:  [[G_PROJ:%.*]] = project_box {{.*}} : ${ let @called(once) @callee_owned () -> () }, 0
// CHECK:  [[CLOSURE_REF:%.*]] = function_ref @$s11called_once22testClosureWithCapture1fyyyXEn_tFyyXOfU_ : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> ()
// CHECK:  [[FN_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[FN_PROJ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[FN_VALUE:%.*]] = load [take] [[FN_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[CLOSURE_WITH_CAPTURE:%.*]] = partial_apply [called_once] [[CLOSURE_REF]]([[FN_VALUE]]) : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> ()
// CHECK:  store [[CLOSURE_WITH_CAPTURE]] to [init] [[G_PROJ]]
// CHECK:  [[X_BOX:%.*]] = alloc_box ${ let @called(once) @callee_owned () -> () }, let, name "x"
// CHECK:  [[BORROWED_X_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[X_BOX]] : ${ let @called(once) @callee_owned () -> () }
// CHECK:  [[X_PROJ:%.*]] = project_box [[BORROWED_X_BOX]] : ${ let @called(once) @callee_owned () -> () }
// CHECK:  [[G_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[G_PROJ]] : $*@called(once) @callee_owned () -> ()
// CHECK:  [[G_FAKE_COPY:%.*]] = load [copy] [[G_ADDR]] : $*@called(once) @callee_owned () -> ()
// CHECK:  store [[G_FAKE_COPY]] to [init] [[X_PROJ]] : $*@called(once) @callee_owned () -> ()
// CHECK:  [[X_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[X_PROJ]] : $*@called(once) @callee_owned () -> ()
// CHECK:  [[X_FAKE_COPY:%.*]] = load [copy] [[X_ADDR]] : $*@called(once) @callee_owned () -> ()
// CHECK:  apply [[X_FAKE_COPY]]() : $@called(once) @callee_owned () -> ()
// CHECK: } // end sil function '$s11called_once22testClosureWithCapture1fyyyXEn_tF'

// Make sure that the closure uses `consumable_and_assignable` to access the capture.
// CHECK-LABEL: sil private [ossa] @$s11called_once22testClosureWithCapture1fyyyXEn_tFyyXOfU_ : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> () {
// CHECK: bb0([[F_CAPTURE:%.*]] : @closureCapture @owned $@noescape @called(once) @callee_owned () -> ()):
// CHECK:  [[F_BOX:%.*]] = alloc_stack $@noescape @called(once) @callee_owned () -> (), var, name "f"
// CHECK:  [[F_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_BOX]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  store [[F_CAPTURE:%.*]] to [init] [[F_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_ADDR_ACCESS:%.*]] = begin_access [read] [unknown] [[F_ADDR_2]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_FAKE_COPY:%.*]] = load [copy] [[F_ADDR_ACCESS]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  end_access [[F_ADDR_ACCESS]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  apply [[F_FAKE_COPY]]() : $@noescape @called(once) @callee_owned () -> ()
// CHECK: } // end sil function '$s11called_once22testClosureWithCapture1fyyyXEn_tFyyXOfU_'
func testClosureWithCapture(f: @called(once) () -> Void) {
  let g = { @called(once) in f() }
  let x = g
  x()
}

// Captured `var`s go through the same "move at formation" path as `let`s:
// forming `g` takes `f`'s current value out via [consumable_and_assignable]
// + load [take]. Reassigning `f` afterward also uses
// [consumable_and_assignable] -- `@called(once)` values always have
// consuming semantics, even for a plain overwrite -- and does not disturb
// the value already moved into `g`.
//
// CHECK-LABEL: sil hidden [ossa] @$s11called_once25testClosureWithVarCaptureyyyyXEnF : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> () {
// CHECK: bb0([[F_PARAM:%.*]] : @owned $@noescape @called(once) @callee_owned () -> ()):
// CHECK:  [[F_PARAM_BOX:%.*]] = alloc_box ${ var @noescape @called(once) @callee_owned () -> () }, var, name "f"
// CHECK:  [[F_PARAM_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[F_PARAM_BOX]] : ${ var @noescape @called(once) @callee_owned () -> () }
// CHECK:  [[F_PARAM_PROJ:%.*]] = project_box [[F_PARAM_BORROW]] : ${ var @noescape @called(once) @callee_owned () -> () }, 0
// CHECK:  store [[F_PARAM]] to [init] [[F_PARAM_PROJ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_BOX:%.*]] = alloc_box ${ var @noescape @called(once) @callee_owned () -> () }, var, name "f"
// CHECK:  [[F_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[F_BOX]] : ${ var @noescape @called(once) @callee_owned () -> () }
// CHECK:  [[F_PROJ:%.*]] = project_box [[F_BORROW]] : ${ var @noescape @called(once) @callee_owned () -> () }, 0
// CHECK:  [[F_PARAM_READ:%.*]] = begin_access [read] [unknown] [[F_PARAM_PROJ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_PARAM_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[F_PARAM_READ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  copy_addr [[F_PARAM_ADDR]] to [init] [[F_PROJ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[G_PROJ:%.*]] = project_box {{.*}} : ${ let @called(once) @callee_owned () -> () }, 0
// CHECK:  [[F_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_PROJ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_VALUE:%.*]] = load [take] [[F_TAKE_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[CLOSURE:%.*]] = partial_apply [called_once] {{.*}}([[F_VALUE]]) : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> ()
// CHECK:  store [[CLOSURE]] to [init] [[G_PROJ]]
// CHECK:  [[NEW_VALUE:%.*]] = apply {{.*}}() : $@convention(thin) () -> @owned @called(once) @callee_owned () -> ()
// CHECK:  [[NEW_VALUE_NOESCAPE:%.*]] = convert_escape_to_noescape [[NEW_VALUE]] : $@called(once) @callee_owned () -> () to $@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_ACCESS:%.*]] = begin_access [modify] [unknown] [[F_PROJ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_WRITE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_ACCESS]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  assign [[NEW_VALUE_NOESCAPE]] to [[F_WRITE_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK: } // end sil function '$s11called_once25testClosureWithVarCaptureyyyyXEnF'

// The closure body's own capture is rebound into a local `var` via a fresh
// `alloc_stack`, not a box - it never escapes further, so there's nothing to
// promote from a box in the first place.
// CHECK-LABEL: sil private [ossa] @$s11called_once25testClosureWithVarCaptureyyyyXEnFyyXOfU_ : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> () {
// CHECK: bb0([[F_CAPTURE:%.*]] : @closureCapture @owned $@noescape @called(once) @callee_owned () -> ()):
// CHECK:  [[F_STACK:%.*]] = alloc_stack $@noescape @called(once) @callee_owned () -> (), var, name "f"
// CHECK:  [[F_INIT_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_STACK]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  store [[F_CAPTURE]] to [init] [[F_INIT_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_INIT_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_READ:%.*]] = begin_access [read] [unknown] [[F_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_FAKE_COPY:%.*]] = load [copy] [[F_READ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  apply [[F_FAKE_COPY]]() : $@noescape @called(once) @callee_owned () -> ()
// CHECK: } // end sil function '$s11called_once25testClosureWithVarCaptureyyyyXEnFyyXOfU_'
func testClosureWithVarCapture(_ f: @called(once) () -> Void) {
  var f = f
  let g = { @called(once) in f() }
  f = makeClosure()
  g()
  f()
}

// Contrast with an ordinary copyable `var` captured by the same closure:
// `count`'s box is captured *by reference* (via `copy_value` on the box
// itself, keeping by-reference semantics so later reads/writes through
// `count` in the closure and the enclosing function stay in sync), while `f`
// is still moved out of its box via [consumable_and_assignable] + load [take]
// at the point the closure literal is formed.
//
// CHECK-LABEL: sil hidden [ossa] @$s11called_once21testMixedCaptureKindsyyyyXEnF : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> () {
// CHECK:  [[F_PROJ:%.*]] = project_box {{.*}} : ${ var @noescape @called(once) @callee_owned () -> () }, 0
// CHECK:  [[COUNT_BOX:%.*]] = alloc_box ${ var Int }, var, name "count"
// CHECK:  [[COUNT_BORROW:%.*]] = begin_borrow [var_decl] [[COUNT_BOX]] : ${ var Int }
// CHECK:  [[COUNT_BOX_COPY:%.*]] = copy_value [[COUNT_BORROW]] : ${ var Int }
// CHECK:  [[F_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_PROJ]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[F_VALUE:%.*]] = load [take] [[F_TAKE_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  partial_apply [called_once] {{.*}}([[COUNT_BOX_COPY]], [[F_VALUE]]) : $@convention(thin) (@guaranteed { var Int }, @owned @noescape @called(once) @callee_owned () -> ()) -> ()
// CHECK: } // end sil function '$s11called_once21testMixedCaptureKindsyyyyXEnF'

// CHECK-LABEL: sil private [ossa] @$s11called_once21testMixedCaptureKindsyyyyXEnFyyXOfU_ : $@convention(thin) (@guaranteed { var Int }, @owned @noescape @called(once) @callee_owned () -> ()) -> () {
// CHECK: bb0([[COUNT_CAPTURE:%.*]] : @closureCapture @guaranteed ${ var Int }, [[F_CAPTURE:%.*]] : @closureCapture @owned $@noescape @called(once) @callee_owned () -> ()):
// CHECK:  [[COUNT_PROJ:%.*]] = project_box [[COUNT_CAPTURE]] : ${ var Int }, 0
// CHECK:  [[F_STACK:%.*]] = alloc_stack $@noescape @called(once) @callee_owned () -> (), var, name "f"
// CHECK:  [[F_INIT_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_STACK]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  store [[F_CAPTURE]] to [init] [[F_INIT_ADDR]] : $*@noescape @called(once) @callee_owned () -> ()
// CHECK:  [[COUNT_ACCESS:%.*]] = begin_access [modify] [unknown] [[COUNT_PROJ]] : $*Int
// CHECK:  end_access [[COUNT_ACCESS]] : $*Int
// CHECK: } // end sil function '$s11called_once21testMixedCaptureKindsyyyyXEnFyyXOfU_'
func testMixedCaptureKinds(_ f: @called(once) () -> Void) {
  var count = 0
  let g = { @called(once) in
    count += 1
    f()
  }
  g()
  _ = count
}

struct Resource: ~Copyable {
  deinit {}
  consuming func use() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s11called_once25testDeferConsumesResourceyyF : $@convention(thin) () -> () {
// CHECK:  [[R_PROJ:%.*]] = project_box {{.*}} : ${ let Resource }, 0
// CHECK:  [[R_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_PROJ]] : $*Resource
// CHECK:  [[R_VALUE:%.*]] = load [take] [[R_TAKE_ADDR]] : $*Resource
// CHECK:  // function_ref $defer #1 () in testDeferConsumesResource()
// CHECK:  [[DEFER_REF:%.*]] = function_ref @$s11called_once25testDeferConsumesResourceyyF6$deferL_yyF : $@convention(thin) (@owned Resource) -> ()
// CHECK:  apply [[DEFER_REF]]([[R_VALUE]]) : $@convention(thin) (@owned Resource) -> ()
// CHECK: } // end sil function '$s11called_once25testDeferConsumesResourceyyF'

// CHECK-LABEL: sil private [ossa] @$s11called_once25testDeferConsumesResourceyyF6$deferL_yyF : $@convention(thin) (@owned Resource) -> () {
// CHECK: bb0([[R_CAPTURE:%.*]] : @closureCapture @owned $Resource):
// CHECK:  [[R_STACK:%.*]] = alloc_stack $Resource, let, name "r"
// CHECK:  [[R_INIT_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_STACK]] : $*Resource
// CHECK:  store [[R_CAPTURE]] to [init] [[R_INIT_ADDR]] : $*Resource
// CHECK:  [[R_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_INIT_ADDR]] : $*Resource
// CHECK:  [[R_FAKE_COPY:%.*]] = load [copy] [[R_ADDR]] : $*Resource
// CHECK:  // function_ref Resource.use()
// CHECK:  [[USE_REF:%.*]] = function_ref @$s11called_once8ResourceV3useyyF : $@convention(method) (@owned Resource) -> ()
// CHECK:  apply [[USE_REF]]([[R_FAKE_COPY]]) : $@convention(method) (@owned Resource) -> ()
// CHECK:  destroy_addr [[R_ADDR]] : $*Resource
// CHECK: } // end sil function '$s11called_once25testDeferConsumesResourceyyF6$deferL_yyF'
func testDeferConsumesResource() {
  let r = Resource()

  defer {
    r.use()
  }

  _ = 42
}
