// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class C {
  func f() -> Int { return 42 }
}

func takeClosure(fn: () -> Int) {}

struct A {
  weak var x: C?
}

// CHECK:    sil hidden @_TF4weak5test0FT1cCS_1C_T_ : $@thin (@owned C) -> () {
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
// CHECK-NEXT: [[TMP:%.*]] = init_enum_data_addr [[OPT]]
// CHECK-NEXT: copy_addr [[C]]#1 to [initialization] [[TMP]] : $*C
// CHECK-NEXT: inject_enum_addr [[OPT]]
// CHECK-NEXT: [[OPTVAL:%.*]] = load [[OPT]]#1 : $*Optional<C>
// CHECK-NEXT: store_weak [[OPTVAL]] to [initialization] [[X]]#1 : $*@sil_weak Optional<C>
// CHECK-NEXT: release_value [[OPTVAL]] : $Optional<C>
// CHECK-NEXT: dealloc_stack [[OPT]]#0

  a.x = c
//   Implicit conversion: call _injectValueIntoOptional
// CHECK-NEXT: [[OPT:%.*]] = alloc_stack $Optional<C>
// CHECK-NEXT: [[TMP:%.*]] = init_enum_data_addr [[OPT]]
// CHECK-NEXT: copy_addr [[C]]#1 to [initialization] [[TMP]] : $*C
// CHECK-NEXT: inject_enum_addr [[OPT]]
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

class CC {
  weak var x: CC?

  // CHECK-LABEL: sil hidden @_TFC4weak2CCcfMS0_FT_S0_
  // CHECK:  [[FOO:%.*]] = alloc_box $Optional<CC>
  // CHECK:  [[X:%.*]] = ref_element_addr %2 : $CC, #CC.x
  // CHECK:  [[VALUE:%.*]] = load_weak [[X]] : $*@sil_weak Optional<CC>
  // CHECK:  store [[VALUE]] to [[FOO]]#1 : $*Optional<CC>
  init() {
    var foo = x
  }
}
