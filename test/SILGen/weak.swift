
// RUN: %target-swift-emit-silgen -module-name weak -Xllvm -sil-full-demangle -enable-sil-ownership %s | %FileCheck %s

class C {
  func f() -> Int { return 42 }
}

func takeClosure(fn: @escaping () -> Int) {}

struct A {
  weak var x: C?
}

// CHECK:    sil hidden @$S4weak5test01cyAA1CC_tF : $@convention(thin) (@guaranteed C) -> () {
func test0(c c: C) {
  var c = c
// CHECK:    bb0(%0 : @guaranteed $C):
// CHECK:      [[C:%.*]] = alloc_box ${ var C }
// CHECK-NEXT: [[PBC:%.*]] = project_box [[C]]

  var a: A
// CHECK:      [[A1:%.*]] = alloc_box ${ var A }
// CHECK:      [[MARKED_A1:%.*]] = mark_uninitialized [var] [[A1]]
// CHECK-NEXT: [[PBA:%.*]] = project_box [[MARKED_A1]]

  weak var x = c
// CHECK:      [[X:%.*]] = alloc_box ${ var @sil_weak Optional<C> }, var, name "x"
// CHECK-NEXT: [[PBX:%.*]] = project_box [[X]]
//   Implicit conversion
// CHECK-NEXT: [[READ:%.*]] = begin_access [read] [unknown] [[PBC]]
// CHECK-NEXT: [[TMP:%.*]] = load [copy] [[READ]] : $*C
// CHECK-NEXT: end_access [[READ]]
// CHECK-NEXT: [[OPTVAL:%.*]] = enum $Optional<C>, #Optional.some!enumelt.1, [[TMP]] : $C
// CHECK-NEXT: store_weak [[OPTVAL]] to [initialization] [[PBX]] : $*@sil_weak Optional<C>
// CHECK-NEXT: destroy_value [[OPTVAL]] : $Optional<C>

  a.x = c
//   Implicit conversion
// CHECK-NEXT: [[READ:%.*]] = begin_access [read] [unknown] [[PBC]]
// CHECK-NEXT: [[TMP:%.*]] = load [copy] [[READ]] : $*C
// CHECK-NEXT:  end_access [[READ]]
// CHECK-NEXT: [[OPTVAL:%.*]] = enum $Optional<C>, #Optional.some!enumelt.1, [[TMP]] : $C

//   Drill to a.x
// CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PBA]]
// CHECK-NEXT: [[A_X:%.*]] = struct_element_addr [[WRITE]] : $*A, #A.x

//   Store to a.x.
// CHECK-NEXT: store_weak [[OPTVAL]] to [[A_X]] : $*@sil_weak Optional<C>
// CHECK-NEXT: destroy_value [[OPTVAL]] : $Optional<C>
}

// <rdar://problem/16871284> silgen crashes on weak capture
// CHECK: closure #1 () -> Swift.Int in weak.testClosureOverWeak() -> ()
// CHECK-LABEL: sil private @$S4weak19testClosureOverWeakyyFSiycfU_ : $@convention(thin) (@guaranteed { var @sil_weak Optional<C> }) -> Int {
// CHECK: bb0(%0 : @guaranteed ${ var @sil_weak Optional<C> }):
// CHECK-NEXT:  %1 = project_box %0
// CHECK-NEXT:  debug_value_addr %1 : $*@sil_weak Optional<C>, var, name "bC", argno 1
// CHECK-NEXT:  [[READ:%.*]] = begin_access [read] [unknown] %1
// CHECK-NEXT:  [[VAL:%.*]] = load_weak [[READ]] : $*@sil_weak Optional<C>
// CHECK-NEXT:  end_access [[READ]]
func testClosureOverWeak() {
  weak var bC = C()
  takeClosure { bC!.f() }
}

class CC {
  weak var x: CC?

  // CHECK-LABEL: sil hidden @$S4weak2CCC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned CC) -> @owned CC {
  // CHECK:  bb0([[SELF:%.*]] : @owned $CC):
  // CHECK:    [[UNINIT_SELF:%.*]] = mark_uninitialized [rootself] [[SELF]] : $CC
  // CHECK:    [[FOO:%.*]] = alloc_box ${ var Optional<CC> }, var, name "foo"
  // CHECK:    [[PB:%.*]] = project_box [[FOO]]
  // CHECK:    [[BORROWED_UNINIT_SELF:%.*]] = begin_borrow [[UNINIT_SELF]]
  // CHECK:    [[X:%.*]] = ref_element_addr [[BORROWED_UNINIT_SELF]] : $CC, #CC.x
  // CHECK:    [[READ:%.*]] = begin_access [read] [dynamic] [[X]] : $*@sil_weak Optional<CC>
  // CHECK:    [[VALUE:%.*]] = load_weak [[READ]] : $*@sil_weak Optional<CC>
  // CHECK:    store [[VALUE]] to [init] [[PB]] : $*Optional<CC>
  // CHECK:    end_borrow [[BORROWED_UNINIT_SELF]] from [[UNINIT_SELF]]
  // CHECK:    destroy_value [[FOO]]
  // CHECK: } // end sil function '$S4weak2CCC{{[_0-9a-zA-Z]*}}fc'
  init() {
    var foo = x
  }
}

func testNoneWeak() {
  weak var x: CC? = nil
  weak var y: CC? = .none
}
