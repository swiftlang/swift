// RUN: %swift -emit-silgen %s | FileCheck %s

class C {
  func f() -> Int { return 42 }
}

func takeClosure(fn: () -> Int) {}

struct A {
  weak var x: C?
}

// CHECK:    sil @_TF4weak5test0FT1cCS_1C_T_ : $@thin (@owned C) -> () {
func test0(var `c: C) {
// CHECK:    bb0(%0 : $C):
// CHECK:      [[C:%.*]] = alloc_box $C

  var a: A
// CHECK:      [[A1:%.*]] = alloc_box $A
// CHECK:      [[A:%.*]] = mark_uninitialized [var] [[A1]]#1

  weak var x = c
// CHECK:      [[X:%.*]] = alloc_box $@sil_weak C
//   Implicit conversion: call _injectValueIntoOptional
// CHECK-NEXT: [[OPT:%.*]] = alloc_stack $Optional<C>
// CHECK-NEXT: [[TMP:%.*]] = alloc_stack $C
// CHECK-NEXT: copy_addr [[C]]#1 to [initialization] [[TMP]]#1 : $*C
// CHECK-NEXT: _injectValueIntoOptional
// CHECK-NEXT: [[INJECT:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NEXT: apply [transparent] [[INJECT]]<{{.*}}>([[OPT]]#1, [[TMP]]#1)
// CHECK-NEXT: dealloc_stack [[TMP]]#0 : $*@local_storage C
// CHECK-NEXT: [[OPTVAL:%.*]] = load [[OPT]]#1 : $*Optional<C>
//   Copy completely unnecessarily into a different temporary.
// CHECK-NEXT: [[OPT2:%.*]] = alloc_stack $Optional<C>
// CHECK-NEXT: store [[OPTVAL]] to [[OPT2]]#1 : $*Optional<C>
//   Turn an Optional<C> into a possibly-nil C:
//     Call _doesOptionalHaveValue.
// CHECK-NEXT: _doesOptionalHaveValue
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FRGSqQ__Bi1_
// CHECK-NEXT: [[T1:%.*]] = apply [transparent] [[T0]]<{{.*}}>([[OPT2]]#1)
// CHECK-NEXT: cond_br [[T1]], [[TRUEBB:.*]], [[FALSEBB:bb[0-9]+]]
//     In the true case, call _getOptionalValue.
// CHECK:    [[TRUEBB]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FGSqQ__Q_
// CHECK-NEXT: [[TMP:%.*]] = alloc_stack $C
// CHECK-NEXT: [[T2:%.*]] = apply [transparent] [[T0]]<{{.*}}>([[TMP]]#1, [[OPT2]]#1)
// CHECK-NEXT: [[T3:%.*]] = load [[TMP]]#1 : $*C
// CHECK-NEXT: dealloc_stack [[TMP]]#0
// CHECK-NEXT: br [[CONTBB:.*]]([[T3]] : $C)
//     In the false case, just create a nil value.
// CHECK:    [[FALSEBB]]:
// CHECK-NEXT: = integer_literal $Builtin.Word, 0
// CHECK-NEXT: = builtin_function_ref "inttoptr_Word" : $@thin @callee_owned (Builtin.Word) -> Builtin.RawPointer
// CHECK-NEXT:  apply
// CHECK-NEXT:  [[T0:%.*]] = raw_pointer_to_ref
//     ...and then destroy optval just for general safety
// CHECK-NEXT: release_value [[OPTVAL]] : $Optional<C>
// CHECK-NEXT: br [[CONTBB]]([[T0]] : $C)
// CHECK:    [[CONTBB]]([[T0:%.*]] : $C)
// CHECK-NEXT: dealloc_stack [[OPT2]]#0
// CHECK-NEXT: store_weak [[T0]] to [initialization] [[X]]#1 : $*@sil_weak C
// CHECK-NEXT: strong_release [[T0]] : $C
// CHECK-NEXT: dealloc_stack [[OPT]]#0

  a.x = c
//   Implicit conversion: call _injectValueIntoOptional
// CHECK-NEXT: [[OPT:%.*]] = alloc_stack $Optional<C>
// CHECK-NEXT: [[TMP:%.*]] = alloc_stack $C
// CHECK-NEXT: copy_addr [[C]]#1 to [initialization] [[TMP]]#1 : $*C
// CHECK-NEXT: _injectValueIntoOptional
// CHECK-NEXT: [[INJECT:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NEXT: apply [transparent] [[INJECT]]<{{.*}}>([[OPT]]#1, [[TMP]]#1)
// CHECK-NEXT: dealloc_stack [[TMP]]#0 : $*@local_storage C
// CHECK-NEXT: [[OPTVAL:%.*]] = load [[OPT]]#1 : $*Optional<C>
//   Drill to a.x
// CHECK-NEXT: [[A_X:%.*]] = struct_element_addr [[A]] : $*A, #A.x
//   Copy completely unnecessarily into a different temporary.
// CHECK-NEXT: [[OPT2:%.*]] = alloc_stack $Optional<C>
// CHECK-NEXT: store [[OPTVAL]] to [[OPT2]]#1 : $*Optional<C>
//   Turn an Optional<C> into a possibly-nil C:
//     Call _doesOptionalHaveValue.
// CHECK-NEXT: _doesOptionalHaveValue
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TFSs22_doesOptionalHaveValueU__FRGSqQ__Bi1_
// CHECK-NEXT: [[T2:%.*]] = apply [transparent] [[T0]]<{{.*}}>([[OPT2]]#1)
// CHECK-NEXT: cond_br [[T2]], [[TRUEBB:.*]], [[FALSEBB:bb[0-9]+]]
//     In the true case, call _getOptionalValue.
// CHECK:    [[TRUEBB]]:
// CHECK:      [[T0:%.*]] = function_ref @_TFSs17_getOptionalValueU__FGSqQ__Q_
// CHECK-NEXT: [[TMP:%.*]] = alloc_stack $C
// CHECK-NEXT: apply [transparent] [[T0]]<{{.*}}>([[TMP]]#1, [[OPT2]]#1)
// CHECK-NEXT: [[T1:%.*]] = load [[TMP]]#1 : $*C
// CHECK-NEXT: dealloc_stack [[TMP]]#0 : $*@local_storage C
// CHECK-NEXT: br [[CONTBB:.*]]([[T1]] : $C)
//     In the false case, just create a nil value.
// CHECK:    [[FALSEBB]]:
// CHECK-NEXT: = integer_literal $Builtin.Word, 0
// CHECK-NEXT: = builtin_function_ref "inttoptr_Word" : $@thin @callee_owned (Builtin.Word) -> Builtin.RawPointer
// CHECK-NEXT:  apply
// CHECK-NEXT:  [[T0:%.*]] = raw_pointer_to_ref
//     ...and then destroy optval just for general safety
// CHECK-NEXT: release_value [[OPTVAL]] : $Optional<C>
// CHECK-NEXT: br [[CONTBB]]([[T0]] : $C)
// CHECK:    [[CONTBB]]([[T0:%.*]] : $C)
// CHECK-NEXT: dealloc_stack [[OPT2]]#0
//   Store to a.x.
// CHECK-NEXT: store_weak [[T0]] to [[A_X]] : $*@sil_weak C
// CHECK-NEXT: strong_release [[T0]] : $C
// CHECK-NEXT: dealloc_stack [[OPT]]#0
}

// <rdar://problem/16871284> silgen crashes on weak capture
// CHECK: weak.(testClosureOverWeak () -> ()).(closure #1)
// CHECK-LABEL: sil shared @_TFF4weak19testClosureOverWeakFT_T_U_FT_Si : $@thin (@owned Builtin.NativeObject, @inout @sil_weak C) -> Int {
// CHECK-NEXT: bb0(%0 : $Builtin.NativeObject, %1 : $*@sil_weak C):
// CHECK-NEXT:  %2 = alloc_stack $Optional<C>
// CHECK-NEXT:  %3 = load_weak %1 : $*@sil_weak C
func testClosureOverWeak() {
  weak var bC = C()
  takeClosure { bC!.f() }
}




