// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s

class C {
  func f() -> Int { return 42 }
}

func takeClosure(fn: @escaping () -> Int) {}

struct A {
  weak var x: C?
}

// CHECK:    sil hidden @_T04weak5test0yAA1CC1c_tF : $@convention(thin) (@owned C) -> () {
func test0(c c: C) {
  var c = c
// CHECK:    bb0(%0 : $C):
// CHECK:      [[C:%.*]] = alloc_box ${ var C }
// CHECK-NEXT: [[PBC:%.*]] = project_box [[C]]

  var a: A
// CHECK:      [[A1:%.*]] = alloc_box ${ var A }
// CHECK-NEXT: [[PBA:%.*]] = project_box [[A1]]
// CHECK:      [[A:%.*]] = mark_uninitialized [var] [[PBA]]

  weak var x = c
// CHECK:      [[X:%.*]] = alloc_box ${ var @sil_weak Optional<C> }, var, name "x"
// CHECK-NEXT: [[PBX:%.*]] = project_box [[X]]
//   Implicit conversion
// CHECK-NEXT: [[TMP:%.*]] = load [copy] [[PBC]] : $*C
// CHECK-NEXT: [[OPTVAL:%.*]] = enum $Optional<C>, #Optional.some!enumelt.1, [[TMP]] : $C
// CHECK-NEXT: store_weak [[OPTVAL]] to [initialization] [[PBX]] : $*@sil_weak Optional<C>
// CHECK-NEXT: destroy_value [[OPTVAL]] : $Optional<C>

  a.x = c
//   Implicit conversion
// CHECK-NEXT: [[TMP:%.*]] = load [copy] [[PBC]] : $*C
// CHECK-NEXT: [[OPTVAL:%.*]] = enum $Optional<C>, #Optional.some!enumelt.1, [[TMP]] : $C

//   Drill to a.x
// CHECK-NEXT: [[A_X:%.*]] = struct_element_addr [[A]] : $*A, #A.x

//   Store to a.x.
// CHECK-NEXT: store_weak [[OPTVAL]] to [[A_X]] : $*@sil_weak Optional<C>
// CHECK-NEXT: destroy_value [[OPTVAL]] : $Optional<C>
}

// <rdar://problem/16871284> silgen crashes on weak capture
// CHECK: weak.(testClosureOverWeak () -> ()).(closure #1)
// CHECK-LABEL: sil shared @_T04weak19testClosureOverWeakyyFSiycfU_ : $@convention(thin) (@owned { var @sil_weak Optional<C> }) -> Int {
// CHECK: bb0(%0 : ${ var @sil_weak Optional<C> }):
// CHECK-NEXT:  %1 = project_box %0
// CHECK-NEXT:  debug_value_addr %1 : $*@sil_weak Optional<C>, var, name "bC", argno 1
// CHECK-NEXT:  %3 = alloc_stack $Optional<C>
// CHECK-NEXT:  %4 = load_weak %1 : $*@sil_weak Optional<C>
// CHECK-NEXT:  store %4 to [init] %3 : $*Optional<C>
func testClosureOverWeak() {
  weak var bC = C()
  takeClosure { bC!.f() }
}

class CC {
  weak var x: CC?

  // CHECK-LABEL: sil hidden @_T04weak2CCC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned CC) -> @owned CC {
  // CHECK:  bb0([[SELF:%.*]] : $CC):
  // CHECK:    [[UNINIT_SELF:%.*]] = mark_uninitialized [rootself] [[SELF]] : $CC
  // CHECK:    [[FOO:%.*]] = alloc_box ${ var Optional<CC> }, var, name "foo"
  // CHECK:    [[PB:%.*]] = project_box [[FOO]]
  // CHECK:    [[BORROWED_UNINIT_SELF:%.*]] = begin_borrow [[UNINIT_SELF]]
  // CHECK:    [[X:%.*]] = ref_element_addr [[BORROWED_UNINIT_SELF]] : $CC, #CC.x
  // CHECK:    [[VALUE:%.*]] = load_weak [[X]] : $*@sil_weak Optional<CC>
  // CHECK:    store [[VALUE]] to [init] [[PB]] : $*Optional<CC>
  // CHECK:    end_borrow [[BORROWED_UNINIT_SELF]] from [[UNINIT_SELF]]
  // CHECK:    destroy_value [[FOO]]
  // CHECK: } // end sil function '_T04weak2CCC{{[_0-9a-zA-Z]*}}fc'
  init() {
    var foo = x
  }
}
