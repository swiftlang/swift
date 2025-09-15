
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name tuples %s | %FileCheck %s
public class C {}

public enum Foo {
  case X(C, Int)
}

// <rdar://problem/16020428>
// CHECK-LABEL: sil hidden [ossa] @$s6tuples8matchFoo1xyAA0C0O_tF
func matchFoo(x x: Foo) {
  switch x {
  case .X(let x):
    ()
  }
}

public protocol P { func foo() }
public struct A : P { public func foo() {} }

func make_int() -> Int { return 0 }
func make_p() -> P { return A() }
func make_xy() -> (x: Int, y: P) { return (make_int(), make_p()) }

// CHECK-LABEL: sil hidden [ossa] @$s6tuples17testShuffleOpaqueyyF
func testShuffleOpaque() {
  // CHECK: [[X:%.*]] = alloc_box ${ var any P }
  // CHECK: [[X_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[X]]
  // CHECK-NEXT: [[PBX:%.*]] = project_box [[X_LIFETIME]]
  // CHECK: [[Y:%.*]] = alloc_box ${ var Int }
  // CHECK-NEXT: [[Y_LIFETIME:%.*]] = begin_borrow [var_decl] [[Y]]
  // CHECK-NEXT: [[PBY:%.*]] = project_box [[Y_LIFETIME]]
  // CHECK-NEXT: [[TMP:%.*]] = alloc_stack $any P

  // CHECK:      [[T0:%.*]] = function_ref @$s6tuples7make_xySi1x_AA1P_p1ytyF
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[TMP]])
  // CHECK-NEXT: copy_addr [take] [[TMP]] to [init] [[PBX]] : $*any P
  // CHECK-NEXT: store [[T1]] to [trivial] [[PBY]]
  // CHECK-NEXT: dealloc_stack [[TMP]]
  var (x,y) : (y:P, x:Int) = make_xy()

  // CHECK-NEXT: [[PAIR:%.*]] = alloc_box ${ var (y: any P, x: Int) }
  // CHECK-NEXT: [[PAIR_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[PAIR]]
  // CHECK-NEXT: [[PBPAIR:%.*]] = project_box [[PAIR_LIFETIME]]
  // CHECK-NEXT: [[TMP:%.*]] = alloc_stack $any P
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @$s6tuples7make_xySi1x_AA1P_p1ytyF
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[TMP]])
  // CHECK-NEXT: [[PAIR_0:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: any P, x: Int), 0
  // CHECK-NEXT: [[PAIR_1:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: any P, x: Int), 1
  // CHECK-NEXT: copy_addr [take] [[TMP]] to [init] [[PAIR_0]] : $*any P
  // CHECK-NEXT: store [[T1]] to [trivial] [[PAIR_1]]
  // CHECK-NEXT: dealloc_stack [[TMP]]
  var pair : (y:P, x:Int) = make_xy()

  // CHECK-NEXT: [[TMP:%.*]] = alloc_stack $any P
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @$s6tuples7make_xySi1x_AA1P_p1ytyF
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[TMP]])
  // CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PBPAIR]] : $*(y: any P, x: Int)
  // CHECK-NEXT: tuple_addr_constructor [assign] [[WRITE]] : $*(y: any P, x: Int) with ([[TMP]] : $*any P, [[T1]] : $Int)
  // CHECK-NEXT: end_access [[WRITE]] : $*(y: any P, x: Int)
  // CHECK-NEXT: dealloc_stack [[TMP]]
  pair = make_xy()
}

// CHECK-LABEL: testShuffleTuple
func testShuffleTuple() {
  // CHECK: [[X:%.*]] = alloc_box ${ var any P }
  // CHECK: [[X_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[X]]
  // CHECK-NEXT: [[PBX:%.*]] = project_box [[X_LIFETIME]]
  // CHECK: [[Y:%.*]] = alloc_box ${ var Int }
  // CHECK-NEXT: [[Y_LIFETIME:%.*]] = begin_borrow [var_decl] [[Y]]
  // CHECK-NEXT: [[PBY:%.*]] = project_box [[Y_LIFETIME]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @$s6tuples8make_intSiyF
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]()
  // CHECK-NEXT: [[TMP:%.*]] = alloc_stack $any P
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @$s6tuples6make_pAA1P_pyF 
  // CHECK-NEXT: apply [[T0]]([[TMP]])
  // CHECK-NEXT: copy_addr [take] [[TMP]] to [init] [[PBX]]
  // CHECK-NEXT: store [[T1]] to [trivial] [[PBY]]
  // CHECK-NEXT: dealloc_stack [[TMP]]
  var (x,y) : (y:P, x:Int) = (x: make_int(), y: make_p())

  // CHECK-NEXT: [[PAIR:%.*]] = alloc_box ${ var (y: any P, x: Int) }
  // CHECK-NEXT: [[PAIR_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[PAIR]]
  // CHECK-NEXT: [[PBPAIR:%.*]] = project_box [[PAIR_LIFETIME]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @$s6tuples8make_intSiyF
  // CHECK-NEXT: [[T1:%.*]] = apply [[T0]]()
  // CHECK-NEXT: [[TMP:%.*]] = alloc_stack $any P
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @$s6tuples6make_pAA1P_pyF 
  // CHECK-NEXT: apply [[T0]]([[TMP]])
  // CHECK-NEXT: [[PAIR_0:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: any P, x: Int), 0
  // CHECK-NEXT: [[PAIR_1:%.*]] = tuple_element_addr [[PBPAIR]] : $*(y: any P, x: Int), 1
  // CHECK-NEXT: copy_addr [take] [[TMP]] to [init] [[PBX]]
  // CHECK-NEXT: store [[T1]] to [trivial] [[PAIR_1]]
  // CHECK-NEXT: dealloc_stack [[TMP]]
  var pair : (y:P, x:Int) = (x: make_int(), y: make_p())

  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @$s6tuples8make_intSiyF
  // CHECK-NEXT: [[INT:%.*]] = apply [[T0]]()
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $any P
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[T0:%.*]] = function_ref @$s6tuples6make_pAA1P_pyF 
  // CHECK-NEXT: apply [[T0]]([[TEMP]])
  // CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PBPAIR]] : $*(y: any P, x: Int)
  // CHECK-NEXT: tuple_addr_constructor [assign] [[WRITE]] : $*(y: any P, x: Int) with ([[TEMP]] : $*any P, [[INT]] : $Int)
  // CHECK-NEXT: end_access [[WRITE]] : $*(y: any P, x: Int)
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  pair = (x: make_int(), y: make_p())
}

enum GenericEnum<T> {
  case one(T)

  static var callback: (T) -> Void { fatalError() }
}

// CHECK-LABEL: $s6tuples16testTupleUnsplatyyF
func testTupleUnsplat() {
  // CHECK: debug_value [[X:%.+]] : $Int, let, name "x"
  // CHECK: debug_value [[Y:%.+]] : $Int, let, name "y"
  let x = 1, y = 2

  // CHECK: [[TUPLE:%.+]] = tuple ([[X]] : $Int, [[Y]] : $Int)
  // CHECK: enum $GenericEnum<(Int, Int)>, #GenericEnum.one!enumelt, [[TUPLE]]
  _ = GenericEnum<(Int, Int)>.one((x, y))

  // CHECK: [[THUNK:%.+]] = function_ref @$sSi_SitIegn_S2iIegyy_TR
  // CHECK: [[REABSTRACTED:%.+]] = partial_apply [callee_guaranteed] [[THUNK]]({{%.+}})
  // CHECK: [[BORROW:%.*]] = begin_borrow [[REABSTRACTED]]
  // CHECK: apply [[BORROW]]([[X]], [[Y]])
  _ = GenericEnum<(Int, Int)>.callback((x, y))
} // CHECK: end sil function '$s6tuples16testTupleUnsplatyyF'

// Make sure that we use a load_borrow instead of a load [take] when RValues are
// formed with isGuaranteed set.
extension P {
  // CHECK-LABEL: sil [ossa] @$s6tuples1PPAAE12immutableUse5tupleyAA1CC5index_x5valuet_tFZ
  // CHECK: bb0([[TUP0:%.*]] : @guaranteed $C, [[TUP1:%.*]] : $*Self
  // Allocate space for the RValue.
  // CHECK:   [[RVALUE:%.*]] = alloc_stack [lexical] [var_decl] $(index: C, value: Self), let, name "tuple"
  //
  // Initialize the RValue. (This is here to help pattern matching).
  // CHECK:   [[ZERO_ADDR:%.*]] = tuple_element_addr [[RVALUE]] : $*(index: C, value: Self), 0
  // CHECK:   [[TUP0_COPY:%.*]] = copy_value [[TUP0]]
  // CHECK:   store [[TUP0_COPY]] to [init] [[ZERO_ADDR]]
  // CHECK:   [[ONE_ADDR:%.*]] = tuple_element_addr [[RVALUE]] : $*(index: C, value: Self), 1
  // CHECK:   copy_addr [[TUP1]] to [init] [[ONE_ADDR]]
  //
  // What we are actually trying to check. Note that there is no actual use of
  // LOADED_CLASS. This is b/c of the nature of the RValue we are working with.
  // CHECK:   [[ZERO_ADDR:%.*]] = tuple_element_addr [[RVALUE]] : $*(index: C, value: Self), 0
  // CHECK:   [[LOADED_CLASS:%.*]] = load_borrow [[ZERO_ADDR]]
  // CHECK:   [[ONE_ADDR:%.*]] = tuple_element_addr [[RVALUE]] : $*(index: C, value: Self), 1
  // CHECK:   apply {{.*}}([[ONE_ADDR]]) : $@convention(witness_method: P)
  // CHECK:   end_borrow [[LOADED_CLASS]]
  // CHECK:   destroy_addr [[RVALUE]]
  // CHECK:   dealloc_stack [[RVALUE]]
  public static func immutableUse(tuple: (index: C, value: Self)) -> () {
    return tuple.value.foo()
  }
}

// CHECK-LABEL: sil [ossa] @$s6tuples15testTupleAssign1xySaySiGz_tF : $@convention(thin) (@inout Array<Int>) -> () {
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] %0 : $*Array<Int>
// function_ref Array.subscript.modify
// CHECK: [[ACCESSOR:%.*]] = function_ref @$sSayxSiciM : $@yield_once @convention(method) <τ_0_0> (Int, @inout Array<τ_0_0>) -> @yields @inout τ_0_0
// CHECK: ([[VALUE:%.*]], [[TOKEN:%.*]]) = begin_apply [[ACCESSOR]]<Int>(%{{.*}}, [[ACCESS]]) : $@yield_once @convention(method) <τ_0_0> (Int, @inout Array<τ_0_0>) -> @yields @inout τ_0_0
// CHECK: assign %{{.*}} to %{{.*}} : $*Int
// CHECK: end_apply [[TOKEN]]
// CHECK: end_access [[ACCESS]] : $*Array<Int>
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] %0 : $*Array<Int>
// function_ref Array.subscript.modify
// CHECK: [[ACCESSOR:%.*]] = function_ref @$sSayxSiciM : $@yield_once @convention(method) <τ_0_0> (Int, @inout Array<τ_0_0>) -> @yields @inout τ_0_0
// CHECK: ([[VALUE:%.*]], [[TOKEN:%.*]]) = begin_apply [[ACCESSOR]]<Int>(%{{.*}}, [[ACCESS]]) : $@yield_once @convention(method) <τ_0_0> (Int, @inout Array<τ_0_0>) -> @yields @inout τ_0_0
// CHECK: assign %{{.*}} to %{{.*}} : $*Int
// CHECK: end_apply [[TOKEN]]
// CHECK: end_access [[ACCESS]] : $*Array<Int>
// CHECK-LABEL: } // end sil function '$s6tuples15testTupleAssign1xySaySiGz_tF'
public func testTupleAssign(x: inout [Int]) {
  (x[0], x[1]) = (0, 1)
}

// CHECK-LABEL: sil [ossa] @$s6tuples16testTupleSubtype1x1y1zyAA1CC_SiSStF : $@convention(thin) (@guaranteed C, Int, @guaranteed String) -> () {
// CHECK:     [[X:%.*]] = copy_value %0 : $C
// CHECK:     [[Z:%.*]] = copy_value %2 : $String
// CHECK:     [[INPUT:%.*]] = tuple $(x: C, y: Int, z: String) ([[X]], %1, [[Z]])
// CHECK:     [[INPUT_MOVE:%.*]] = move_value [lexical] [var_decl] [[INPUT]] : $(x: C, y: Int, z: String)
// CHECK:     [[OUTPUT:%.*]] = alloc_stack [lexical] [var_decl] $(y: Optional<Int>, z: Any, x: AnyObject)
// CHECK:     [[INPUT_BORROW:%.*]] = begin_borrow [[INPUT_MOVE]] : $(x: C, y: Int, z: String)
// CHECK:     [[INPUT_COPY:%.*]] = copy_value [[INPUT_BORROW]] : $(x: C, y: Int, z: String)
// CHECK:     ([[X:%.*]], [[Y:%.*]], [[Z:%.*]]) = destructure_tuple [[INPUT_COPY]] : $(x: C, y: Int, z: String)
// CHECK:     [[Y_ADDR:%.*]] = tuple_element_addr [[OUTPUT]] : $*(y: Optional<Int>, z: Any, x: AnyObject), 0
// CHECK:     [[Z_ADDR:%.*]] = tuple_element_addr [[OUTPUT]] : $*(y: Optional<Int>, z: Any, x: AnyObject), 1
// CHECK:     [[X_ADDR:%.*]] = tuple_element_addr [[OUTPUT]] : $*(y: Optional<Int>, z: Any, x: AnyObject), 2
// CHECK:     [[NEW_Y:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, [[Y]] : $Int
// CHECK:     store [[NEW_Y]] to [trivial] [[Y_ADDR]] : $*Optional<Int>
// CHECK:     [[NEW_Z:%.*]] = init_existential_addr [[Z_ADDR]] : $*Any, $String
// CHECK:     store [[Z]] to [init] [[NEW_Z]] : $*String
// CHECK:     [[NEW_X:%.*]] = init_existential_ref [[X]] : $C : $C, $AnyObject
// CHECK:     store [[NEW_X]] to [init] [[X_ADDR]] : $*AnyObject
// CHECK:     end_borrow [[INPUT_BORROW]] : $(x: C, y: Int, z: String)
// CHECK:     destroy_addr [[OUTPUT]] : $*(y: Optional<Int>, z: Any, x: AnyObject)
// CHECK:     dealloc_stack [[OUTPUT]] : $*(y: Optional<Int>, z: Any, x: AnyObject)
// CHECK:     destroy_value [[INPUT_MOVE]] : $(x: C, y: Int, z: String)

public func testTupleSubtype(x: C, y: Int, z: String) {
  let input = (x: x, y: y, z: z)
  let output: (y: Int?, z: Any, x: AnyObject) = input
}
