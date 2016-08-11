// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
class C {}

enum Foo {
  case X(C, Int)
}

// <rdar://problem/16020428>
// CHECK-LABEL: sil hidden @_TF6tuples8matchFooFT1xOS_3Foo_T_
func matchFoo(x x: Foo) {
  switch x {
  case .X(let x):
    ()
  }
}

protocol P { func foo() }
struct A : P { func foo() {} }

func make_int() -> Int { return 0 }
func make_p() -> P { return A() }
func make_xy() -> (x: Int, y: P) { return (make_int(), make_p()) }

// CHECK-LABEL: sil hidden @_TF6tuples17testShuffleOpaqueFT_T_
func testShuffleOpaque() {
  // CHECK: [[X:%.*]] = alloc_box $P
  // CHECK-NEXT: [[PBX:%.*]] = project_box [[X]]
  // CHECK: [[Y:%.*]] = alloc_box $Int
  // CHECK-NEXT: [[PBY:%.*]] = project_box [[Y]]

  // CHECK:      [[T0:%.*]] = function_ref @_TF6tuples7make_xyFT_T1xSi1yPS_1P__
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[PBX]])
  // CHECK-NEXT: store [[T1]] to [[PBY]]
  var (x,y) : (y:P, x:Int) = make_xy()

  // CHECK-NEXT: [[PAIR:%.*]] = alloc_box $(y: P, x: Int)
  // CHECK-NEXT: [[PBPAIR:%.*]] = project_box [[PAIR]]
  // CHECK-NEXT: [[PAIR_0:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: P, x: Int), 0
  // CHECK-NEXT: [[PAIR_1:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: P, x: Int), 1
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @_TF6tuples7make_xyFT_T1xSi1yPS_1P__
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[PAIR_0]])
  // CHECK-NEXT: store [[T1]] to [[PAIR_1]]
  var pair : (y:P, x:Int) = make_xy()

  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @_TF6tuples7make_xyFT_T1xSi1yPS_1P__
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $P
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[TEMP]])
  // CHECK-NEXT: [[PAIR_0:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: P, x: Int), 0
  // CHECK-NEXT: copy_addr [take] [[TEMP]] to [[PAIR_0]]
  // CHECK-NEXT: [[PAIR_1:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: P, x: Int), 1
  // CHECK-NEXT: assign [[T1]] to [[PAIR_1]]
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  pair = make_xy()
}

// CHECK-LABEL: testShuffleTuple
func testShuffleTuple() {
  // CHECK: [[X:%.*]] = alloc_box $P
  // CHECK-NEXT: [[PBX:%.*]] = project_box [[X]]
  // CHECK: [[Y:%.*]] = alloc_box $Int
  // CHECK-NEXT: [[PBY:%.*]] = project_box [[Y]]

  // CHECK:      [[T0:%.*]] = function_ref @_TF6tuples8make_intFT_Si
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]()
  // CHECK-NEXT: store [[T1]] to [[PBY]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @_TF6tuples6make_pFT_PS_1P_ 
  // CHECK-NEXT: apply [[T0]]([[PBX]])
  var (x,y) : (y:P, x:Int) = (x: make_int(), y: make_p())

  // CHECK-NEXT: [[PAIR:%.*]] = alloc_box $(y: P, x: Int)
  // CHECK-NEXT: [[PBPAIR:%.*]] = project_box [[PAIR]]
  // CHECK-NEXT: [[PAIR_0:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: P, x: Int), 0
  // CHECK-NEXT: [[PAIR_1:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: P, x: Int), 1
  // CHECK-NEXT: // function_ref
  // CHECK:      [[T0:%.*]] = function_ref @_TF6tuples8make_intFT_Si
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]()
  // CHECK-NEXT: store [[T1]] to [[PAIR_1]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @_TF6tuples6make_pFT_PS_1P_ 
    // CHECK-NEXT: apply [[T0]]([[PAIR_0]])
  var pair : (y:P, x:Int) = (x: make_int(), y: make_p())

  //   This isn't really optimal; we should be evaluating make_p directly
  //   into the temporary.
  // CHECK-NEXT: // function_ref
  // CHECK:      [[T0:%.*]] = function_ref @_TF6tuples8make_intFT_Si
  // CHECK-NEXT: [[INT:%.*]] = apply [[T0]]()
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @_TF6tuples6make_pFT_PS_1P_ 
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $P
  // CHECK-NEXT: apply [[T0]]([[TEMP]])
  // CHECK-NEXT: [[PAIR_0:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: P, x: Int), 0
  // CHECK-NEXT: copy_addr [take] [[TEMP]] to [[PAIR_0]]
  // CHECK-NEXT: [[PAIR_1:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: P, x: Int), 1
  // CHECK-NEXT: assign [[INT]] to [[PAIR_1]]
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  pair = (x: make_int(), y: make_p())
}
