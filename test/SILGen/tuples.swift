// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s
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
  // CHECK: [[Y:%.*]] = alloc_box $Int

  // CHECK:      [[T0:%.*]] = function_ref @_TF6tuples7make_xyFT_T1xSi1yPS_1P__
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $(x: Int, y: P)
  // CHECK-NEXT: apply [[T0]]([[TEMP]]#1)
  // CHECK-NEXT: [[T0:%.*]] = tuple_element_addr [[TEMP]]#1 : $*(x: Int, y: P), 0
  // CHECK-NEXT: [[T1:%.*]] = load [[T0]] : $*Int
  // CHECK-NEXT: [[T2:%.*]] = tuple_element_addr [[TEMP]]#1 : $*(x: Int, y: P), 1
  // CHECK-NEXT: store [[T1]] to [[Y]]#1
  // CHECK-NEXT: copy_addr [take] [[T2]] to [initialization] [[X]]#1
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  var (x,y) : (y:P, x:Int) = make_xy()

  // CHECK-NEXT: [[PAIR:%.*]] = alloc_box $(y: P, x: Int)
  // CHECK-NEXT: [[PAIR_0:%.*]] = tuple_element_addr [[PAIR]]#1 : $*(y: P, x: Int), 0
  // CHECK-NEXT: [[PAIR_1:%.*]] = tuple_element_addr [[PAIR]]#1 : $*(y: P, x: Int), 1
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @_TF6tuples7make_xyFT_T1xSi1yPS_1P__
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $(x: Int, y: P)
  // CHECK-NEXT: apply [[T0]]([[TEMP]]#1)
  // CHECK-NEXT: [[T0:%.*]] = tuple_element_addr [[TEMP]]#1 : $*(x: Int, y: P), 0
  // CHECK-NEXT: [[T1:%.*]] = load [[T0]] : $*Int
  // CHECK-NEXT: [[T2:%.*]] = tuple_element_addr [[TEMP]]#1 : $*(x: Int, y: P), 1
  // CHECK-NEXT: store [[T1]] to [[PAIR_1]]
  // CHECK-NEXT: copy_addr [take] [[T2]] to [initialization] [[PAIR_0]]
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  var pair : (y:P, x:Int) = make_xy()

  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @_TF6tuples7make_xyFT_T1xSi1yPS_1P__
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $(x: Int, y: P)
  // CHECK-NEXT: apply [[T0]]([[TEMP]]#1)
  // CHECK-NEXT: [[T0:%.*]] = tuple_element_addr [[TEMP]]#1 : $*(x: Int, y: P), 0
  // CHECK-NEXT: [[T1:%.*]] = load [[T0]] : $*Int
  // CHECK-NEXT: [[T2:%.*]] = tuple_element_addr [[TEMP]]#1 : $*(x: Int, y: P), 1
  // CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $(y: P, x: Int)
  // CHECK-NEXT: [[TEMP2_0:%.*]] = tuple_element_addr [[TEMP2]]#1 : $*(y: P, x: Int), 0
  // CHECK-NEXT: copy_addr [take] [[T2]] to [initialization] [[TEMP2_0]]
  // CHECK-NEXT: [[TEMP2_1:%.*]] = tuple_element_addr [[TEMP2]]#1 : $*(y: P, x: Int), 1
  // CHECK-NEXT: store [[T1]] to [[TEMP2_1]]
  // CHECK-NEXT: copy_addr [take] [[TEMP2]]#1 to [[PAIR]]#1
  // CHECK-NEXT: dealloc_stack [[TEMP2]]
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  pair = make_xy()
}

func testShuffleTuple() {
  // CHECK: [[X:%.*]] = alloc_box $P
  // CHECK: [[Y:%.*]] = alloc_box $Int

  // CHECK:      [[T0:%.*]] = function_ref @_TF6tuples8make_intFT_Si
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]()
  // CHECK-NEXT: store [[T1]] to [[Y]]#1
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @_TF6tuples6make_pFT_PS_1P_ 
  // CHECK-NEXT: apply [[T0]]([[X]]#1)
  var (x,y) : (y:P, x:Int) = (x: make_int(), y: make_p())

  // CHECK-NEXT: [[PAIR:%.*]] = alloc_box $(y: P, x: Int)
  // CHECK-NEXT: [[PAIR_0:%.*]] = tuple_element_addr [[PAIR]]#1 : $*(y: P, x: Int), 0
  // CHECK-NEXT: [[PAIR_1:%.*]] = tuple_element_addr [[PAIR]]#1 : $*(y: P, x: Int), 1
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
  // CHECK-NEXT: apply [[T0]]([[TEMP]]#1)
  // CHECK-NEXT: [[TEMP2:%.*]] = alloc_stack $(y: P, x: Int)
  // CHECK-NEXT: [[TEMP2_0:%.*]] = tuple_element_addr [[TEMP2]]#1 : $*(y: P, x: Int), 0
  // CHECK-NEXT: copy_addr [take] [[TEMP]]#1 to [initialization] [[TEMP2_0]]
  // CHECK-NEXT: [[TEMP2_1:%.*]] = tuple_element_addr [[TEMP2]]#1 : $*(y: P, x: Int), 1
  // CHECK-NEXT: store [[INT]] to [[TEMP2_1]]
  // CHECK-NEXT: copy_addr [take] [[TEMP2]]#1 to [[PAIR]]#1
  // CHECK-NEXT: dealloc_stack [[TEMP2]]
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  pair = (x: make_int(), y: make_p())
}