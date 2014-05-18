// RUN: %swift -emit-silgen %s | FileCheck %s

class C {
  func f() -> Int { return 42 }
}

func takeClosure(fn: () -> Int) {}

struct A {
  weak var x: C?
}

// CHECK:    sil @_TF4weak5test0FT1cCS_1C_T_ : $@thin (@owned C) -> () {
func test0(var #c: C) {
// CHECK:    bb0(%0 : $C):
// CHECK:      [[C:%.*]] = alloc_box $C

  var a: A
// CHECK:      [[A1:%.*]] = alloc_box $A
// CHECK:      [[A:%.*]] = mark_uninitialized [var] [[A1]]#1

  weak var x = c
// CHECK:      [[X:%.*]] = alloc_box $@sil_weak Optional<C>  // var x
//   Implicit conversion: call _injectValueIntoOptional
// CHECK-NEXT: [[OPT:%.*]] = alloc_stack $Optional<C>
// CHECK-NEXT: [[TMP:%.*]] = alloc_stack $C
// CHECK-NEXT: copy_addr [[C]]#1 to [initialization] [[TMP]]#1 : $*C
// CHECK-NEXT: _injectValueIntoOptional
// CHECK-NEXT: [[INJECT:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK-NEXT: apply [transparent] [[INJECT]]<{{.*}}>([[OPT]]#1, [[TMP]]#1)
// CHECK-NEXT: dealloc_stack [[TMP]]#0 : $*@local_storage C
// CHECK-NEXT: [[OPTVAL:%.*]] = load [[OPT]]#1 : $*Optional<C>
// CHECK-NEXT: store_weak [[OPTVAL]] to [initialization] [[X]]#1 : $*@sil_weak Optional<C>
// CHECK-NEXT: release_value [[OPTVAL]] : $Optional<C>
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


//   Store to a.x.
// CHECK-NEXT: store_weak [[OPTVAL]] to [[A_X]] : $*@sil_weak Optional<C>
// CHECK-NEXT: release_value [[OPTVAL]] : $Optional<C>
// CHECK-NEXT: dealloc_stack [[OPT]]#0
}

// <rdar://problem/16871284> silgen crashes on weak capture
// CHECK: weak.(testClosureOverWeak () -> ()).(closure #1)
// CHECK-LABEL: sil shared @_TFF4weak19testClosureOverWeakFT_T_U_FT_Si : $@thin (@owned Builtin.NativeObject, @inout @sil_weak Optional<C>) -> Int {
// CHECK-NEXT: bb0(%0 : $Builtin.NativeObject, %1 : $*@sil_weak Optional<C>):
// CHECK-NEXT:  %2 = alloc_stack $Optional<C>
// CHECK-NEXT:  %3 = load_weak %1 : $*@sil_weak Optional<C>
// CHECK-NEXT:  store %3 to %2#1 : $*Optional<C>
func testClosureOverWeak() {
  weak var bC = C()
  takeClosure { bC!.f() }
}




