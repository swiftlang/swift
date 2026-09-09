// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

struct Resource: ~Copyable {
  deinit {}
  consuming func use() {}
}

struct Box<Wrapped: ~Copyable>: ~Copyable {
  var value: Wrapped? = nil
  consuming func take() -> Wrapped { value! }
}

extension Box: Copyable where Wrapped: Copyable {
}

protocol Usable: ~Copyable {
  consuming func use()
}

func runOnce(_ f: @called(once) () -> Void) {
  f()
}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_consuming_method10testSimpleyyAA8ResourceVnF : $@convention(thin) (@owned Resource) -> () {
// CHECK: alloc_box ${ let @called(once) @callee_owned () -> () }, let, name "f1"
// CHECK: [[F1_VALUE:%.*]] = load [take] {{%.*}} : $*Resource
// CHECK: [[F1_REF:%.*]] = function_ref @$s28called_once_consuming_method10testSimpleyyAA8ResourceVnFyyXOADncfu_ : $@convention(thin) (@owned Resource) -> @owned @called(once) @callee_owned () -> ()
// CHECK: apply [[F1_REF]]([[F1_VALUE]]) : $@convention(thin) (@owned Resource) -> @owned @called(once) @callee_owned () -> ()
// CHECK: [[F2_REF:%.*]] = function_ref @$s28called_once_consuming_method10testSimpleyyAA8ResourceVnFyyXOADncfu1_ : $@convention(thin) (@owned Resource) -> @owned @called(once) @callee_owned () -> ()
// CHECK: [[F2_THICK:%.*]] = thin_to_thick_function [[F2_REF]] : $@convention(thin) (@owned Resource) -> @owned @called(once) @callee_owned () -> () to $@callee_guaranteed (@owned Resource) -> @owned @called(once) @callee_owned () -> ()
// CHECK: move_value [lexical] [var_decl] [[F2_THICK]] : $@callee_guaranteed (@owned Resource) -> @owned @called(once) @callee_owned () -> ()
// CHECK: } // end sil function '$s28called_once_consuming_method10testSimpleyyAA8ResourceVnF'

// CHECK-LABEL: sil private [ossa] @$s28called_once_consuming_method10testSimpleyyAA8ResourceVnFyyXOADncfu_ : $@convention(thin) (@owned Resource) -> @owned @called(once) @callee_owned () -> () {
// CHECK: [[THUNK:%.*]] = function_ref @$s28called_once_consuming_method10testSimpleyyAA8ResourceVnFyyXOADncfu_yyXOfu0_ : $@convention(thin) (@owned Resource) -> ()
// CHECK: [[VALUE:%.*]] = load [take] {{%.*}} : $*Resource
// CHECK: [[CLOSURE:%.*]] = partial_apply [called_once] [[THUNK]]([[VALUE]]) : $@convention(thin) (@owned Resource) -> ()
// CHECK: return [[CLOSURE]] : $@called(once) @callee_owned () -> ()
// CHECK: } // end sil function '$s28called_once_consuming_method10testSimpleyyAA8ResourceVnFyyXOADncfu_'
func testSimple(_ r: consuming Resource) {
  let f1 = r.use
  f1()

  let f2 = Resource.use
  _ = f2
}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_consuming_method31testCopyableViaGenericParameteryyAA3BoxVySiGnF : $@convention(thin) (Box<Int>) -> () {
// CHECK-NOT: @called(once)
// CHECK: } // end sil function '$s28called_once_consuming_method31testCopyableViaGenericParameteryyAA3BoxVySiGnF'
func testCopyableViaGenericParameter(_ b: consuming Box<Int>) {
  let f = b.take
  _ = f()
}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_consuming_method34testNonCopyableViaGenericParameter_2b2yAA3BoxVyxGn_AFntRi_zlF : $@convention(thin) <T where T : ~Copyable> (@in Box<T>, @in Box<T>) -> () {
// CHECK: alloc_box $<τ_0_0 where τ_0_0 : ~Copyable> { let @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0> } <T>, let, name "f1"
// CHECK: [[F1_REF:%.*]] = function_ref @$s28called_once_consuming_method34testNonCopyableViaGenericParameter_2b2yAA3BoxVyxGn_AFntRi_zlFxyXOAFncfu_ : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in Box<τ_0_0>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>
// CHECK: apply [[F1_REF]]<T>({{%.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in Box<τ_0_0>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>
// CHECK: [[UNAPPLIED_REF:%.*]] = function_ref @$s28called_once_consuming_method34testNonCopyableViaGenericParameter_2b2yAA3BoxVyxGn_AFntRi_zlFxyXOAFncfu1_ : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in Box<τ_0_0>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>
// CHECK: [[UNAPPLIED_PARTIAL:%.*]] = partial_apply [callee_guaranteed] [[UNAPPLIED_REF]]<T>() : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in Box<τ_0_0>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>
// CHECK: [[UNAPPLIED:%.*]] = convert_function [[UNAPPLIED_PARTIAL]] : $@callee_guaranteed (@in Box<T>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <T> to $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in Box<τ_0_0>) -> (@owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_1>) for <T, T>
// CHECK: [[UNAPPLIED_MOVED:%.*]] = move_value [lexical] [var_decl] [[UNAPPLIED]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in Box<τ_0_0>) -> (@owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_1>) for <T, T>
// CHECK: debug_value [[UNAPPLIED_MOVED]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in Box<τ_0_0>) -> (@owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_1>) for <T, T>, let, name "unapplied"
// CHECK: alloc_box $<τ_0_0 where τ_0_0 : ~Copyable> { let @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0> } <T>, let, name "f2"
// CHECK: [[F2_VALUE:%.*]] = apply {{%.*}}({{%.*}}) : $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in Box<τ_0_0>) -> (@owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_1>) for <T, T>
// CHECK: store [[F2_VALUE]] to [init] {{%.*}} : $*@called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <T>
// CHECK: } // end sil function '$s28called_once_consuming_method34testNonCopyableViaGenericParameter_2b2yAA3BoxVyxGn_AFntRi_zlF'

// CHECK-LABEL: sil private [ossa] @$s28called_once_consuming_method34testNonCopyableViaGenericParameter_2b2yAA3BoxVyxGn_AFntRi_zlFxyXOAFncfu_ : $@convention(thin) <T where T : ~Copyable> (@in Box<T>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <T> {
// CHECK: [[THUNK:%.*]] = function_ref @$s28called_once_consuming_method34testNonCopyableViaGenericParameter_2b2yAA3BoxVyxGn_AFntRi_zlFxyXOAFncfu_xyXOfu0_ : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in Box<τ_0_0>) -> @out τ_0_0
// CHECK: [[CLOSURE:%.*]] = partial_apply [called_once] [[THUNK]]<T>({{%.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in Box<τ_0_0>) -> @out τ_0_0
// CHECK: [[SUBSTITUTED:%.*]] = convert_function [[CLOSURE]] : $@called(once) @callee_owned () -> @out T to $@called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <T>
// CHECK: return [[SUBSTITUTED]] : $@called(once) @callee_owned @substituted <τ_0_0> () -> @out τ_0_0 for <T>
// CHECK: } // end sil function '$s28called_once_consuming_method34testNonCopyableViaGenericParameter_2b2yAA3BoxVyxGn_AFntRi_zlFxyXOAFncfu_'
func testNonCopyableViaGenericParameter<T: ~Copyable>(_ b1: consuming Box<T>, b2: consuming Box<T>) {
  let f1 = b1.take
  _ = f1()

  let unapplied = Box<T>.take
  let f2 = unapplied(b2)
  _ = f2()
}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_consuming_method23testProtocolRequirementyyxnAA6UsableRzRi_zlF : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> () {
// CHECK: alloc_box ${ let @called(once) @callee_owned () -> () }, let, name "f"
// CHECK: } // end sil function '$s28called_once_consuming_method23testProtocolRequirementyyxnAA6UsableRzRi_zlF'

// CHECK-LABEL: sil private [ossa] @$s28called_once_consuming_method23testProtocolRequirementyyxnAA6UsableRzRi_zlFyyXOxncfu_yyXOfu0_ : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> () {
// CHECK: witness_method $T, #Usable.use : <Self where Self : Usable, Self : ~Copyable> (consuming Self) -> () -> ()
// CHECK: } // end sil function '$s28called_once_consuming_method23testProtocolRequirementyyxnAA6UsableRzRi_zlFyyXOxncfu_yyXOfu0_'
func testProtocolRequirement<T: Usable & ~Copyable>(_ x: consuming T) {
  let f = x.use
  f()
}

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_consuming_method25testUseOfCalledOnceMethodyyAA8ResourceVnF : $@convention(thin) (@owned Resource) -> () {
// CHECK: [[CLOSURE_REF:%.*]] = function_ref @$s28called_once_consuming_method25testUseOfCalledOnceMethodyyAA8ResourceVnFyyXOADncfu_ : $@convention(thin) (@owned Resource) -> @owned @called(once) @callee_owned () -> ()
// CHECK: [[CLOSURE:%.*]] = apply [[CLOSURE_REF]]({{%.*}}) : $@convention(thin) (@owned Resource) -> @owned @called(once) @callee_owned () -> ()
// CHECK: [[NOESCAPE:%.*]] = convert_escape_to_noescape [[CLOSURE]] : $@called(once) @callee_owned () -> () to $@noescape @called(once) @callee_owned () -> ()
// CHECK: function_ref @$s28called_once_consuming_method7runOnceyyyyXEnF
// CHECK: apply {{%.*}}([[NOESCAPE]]) : $@convention(thin) (@owned @noescape @called(once) @callee_owned () -> ()) -> ()
// CHECK: } // end sil function '$s28called_once_consuming_method25testUseOfCalledOnceMethodyyAA8ResourceVnF'
func testUseOfCalledOnceMethod(_ r: consuming Resource) {
  runOnce(r.use)
}

func pick<T: ~Copyable>(_ b: Bool) -> Box<T> {
  .init()
}
func pick(_: String) -> String { "" }

// CHECK-LABEL: sil hidden [ossa] @$s28called_once_consuming_method26testJoinWithDependentTypesyxSbRi_zlF : $@convention(thin) <T where T : ~Copyable> (Bool) -> @out T {
// CHECK: alloc_box $<τ_0_0 where τ_0_0 : ~Copyable> { let Box<τ_0_0> } <T>, let, name "f"
// CHECK: cond_br {{%.*}}, [[TRUE_BB:bb[0-9]+]], [[FALSE_BB:bb[0-9]+]]
//
// CHECK: [[TRUE_BB]]:
// CHECK: [[PICK_REF:%.*]] = function_ref @$s28called_once_consuming_method4pickyAA3BoxVyxGSbRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (Bool) -> @out Box<τ_0_0>
// CHECK: apply [[PICK_REF]]<Box<T>>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (Bool) -> @out Box<τ_0_0>
// CHECK: [[TRUE_REF:%.*]] = function_ref @$s28called_once_consuming_method26testJoinWithDependentTypesyxSbRi_zlFAA3BoxVyxGyXOADyAEGncfu_ : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in Box<Box<τ_0_0>>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out Box<τ_0_0> for <τ_0_0>
// CHECK: [[TRUE_VALUE:%.*]] = apply [[TRUE_REF]]<T>({{%.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in Box<Box<τ_0_0>>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> () -> @out Box<τ_0_0> for <τ_0_0>
// CHECK: br [[JOIN_BB:bb[0-9]+]]([[TRUE_VALUE]] : $@called(once) @callee_owned @substituted <τ_0_0> () -> @out Box<τ_0_0> for <T>)
//
// CHECK: [[FALSE_BB]]:
// CHECK: [[FALSE_REF:%.*]] = function_ref @$s28called_once_consuming_method26testJoinWithDependentTypesyxSbRi_zlFAA3BoxVyxGyXOfU_ : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> () -> @out Box<τ_0_0>
// CHECK: [[FALSE_CLOSURE:%.*]] = partial_apply [called_once] [[FALSE_REF]]<T>() : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> () -> @out Box<τ_0_0>
// CHECK: [[FALSE_VALUE:%.*]] = convert_function [[FALSE_CLOSURE]] : $@called(once) @callee_owned () -> @out Box<T> to $@called(once) @callee_owned @substituted <τ_0_0> () -> @out Box<τ_0_0> for <T>
// CHECK: br [[JOIN_BB]]([[FALSE_VALUE]] : $@called(once) @callee_owned @substituted <τ_0_0> () -> @out Box<τ_0_0> for <T>)
//
// CHECK: [[JOIN_BB]]([[JOINED:%.*]] : @owned $@called(once) @callee_owned @substituted <τ_0_0> () -> @out Box<τ_0_0> for <T>):
// CHECK: apply [[JOINED]]({{%.*}}) : $@called(once) @callee_owned @substituted <τ_0_0> () -> @out Box<τ_0_0> for <T>
// CHECK: } // end sil function '$s28called_once_consuming_method26testJoinWithDependentTypesyxSbRi_zlF'
func testJoinWithDependentTypes<T: ~Copyable>(_ b: Bool) -> T {
  let f: Box<T> = (b ? pick(b).take : { @called(once) in .init() })()
  _ = f
}
