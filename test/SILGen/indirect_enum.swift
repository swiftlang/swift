// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

indirect enum TreeA<T> {
  case Nil
  case Leaf(T)
  case Branch(left: TreeA<T>, right: TreeA<T>)
}

// CHECK-LABEL: sil hidden @_T013indirect_enum11TreeA_casesyx_AA0C1AOyxG1lADyxG1rtlF : $@convention(thin) <T> (@in T, @owned TreeA<T>, @owned TreeA<T>) -> () {
func TreeA_cases<T>(_ t: T, l: TreeA<T>, r: TreeA<T>) {
// CHECK: bb0([[ARG1:%.*]] : $*T, [[ARG2:%.*]] : $TreeA<T>, [[ARG3:%.*]] : $TreeA<T>):
// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[NIL:%.*]] = enum $TreeA<T>, #TreeA.Nil!enumelt
// CHECK-NEXT:    destroy_value [[NIL]]
  let _ = TreeA<T>.Nil

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var τ_0_0 } <T>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    copy_addr [[ARG1]] to [initialization] [[PB]]
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeA<T>, #TreeA.Leaf!enumelt.1, [[BOX]]
// CHECK-NEXT:    destroy_value [[LEAF]]
  let _ = TreeA<T>.Leaf(t)

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var τ_0_0 } <(left: TreeA<T>, right: TreeA<T>)>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]] : $*(left: TreeA<T>, right: TreeA<T>), 0
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]] : $*(left: TreeA<T>, right: TreeA<T>), 1
// CHECK-NEXT:    [[BORROWED_ARG2:%.*]] = begin_borrow [[ARG2]]
// CHECK-NEXT:    [[ARG2_COPY:%.*]] = copy_value [[BORROWED_ARG2]]
// CHECK-NEXT:    store [[ARG2_COPY]] to [init] [[LEFT]]
// CHECK-NEXT:    [[BORROWED_ARG3:%.*]] = begin_borrow [[ARG3]]
// CHECK-NEXT:    [[ARG3_COPY:%.*]] = copy_value [[BORROWED_ARG3]]
// CHECK-NEXT:    store [[ARG3_COPY]] to [init] [[RIGHT]]
// CHECK-NEXT:    [[BRANCH:%.*]] = enum $TreeA<T>, #TreeA.Branch!enumelt.1, [[BOX]]
// CHECK-NEXT:    destroy_value [[BRANCH]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG3]] from [[ARG3]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG2]] from [[ARG2]]
// CHECK-NEXT:    destroy_value [[ARG3]]
// CHECK-NEXT:    destroy_value [[ARG2]]
// CHECK-NEXT:    destroy_addr [[ARG1]]
  let _ = TreeA<T>.Branch(left: l, right: r)

}
// CHECK: // end sil function '_T013indirect_enum11TreeA_casesyx_AA0C1AOyxG1lADyxG1rtlF'


// CHECK-LABEL: sil hidden @_T013indirect_enum16TreeA_reabstractySiSicF : $@convention(thin) (@owned @callee_owned (Int) -> Int) -> () {
func TreeA_reabstract(_ f: @escaping (Int) -> Int) {
// CHECK: bb0([[ARG:%.*]] : $@callee_owned (Int) -> Int):
// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeA<(Int) -> Int>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var τ_0_0 } <@callee_owned (@in Int) -> @out Int>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT:    [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:         [[THUNK:%.*]] = function_ref @_T0SiSiIxyd_SiSiIxir_TR
// CHECK-NEXT:    [[FN:%.*]] = partial_apply [[THUNK]]([[ARG_COPY]])
// CHECK-NEXT:    store [[FN]] to [init] [[PB]]
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeA<(Int) -> Int>, #TreeA.Leaf!enumelt.1, [[BOX]]
// CHECK-NEXT:    destroy_value [[LEAF]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT:    destroy_value [[ARG]]
// CHECK: return
  let _ = TreeA<(Int) -> Int>.Leaf(f)
}
// CHECK: } // end sil function '_T013indirect_enum16TreeA_reabstractySiSicF'

enum TreeB<T> {
  case Nil
  case Leaf(T)
  indirect case Branch(left: TreeB<T>, right: TreeB<T>)
}

// CHECK-LABEL: sil hidden @_T013indirect_enum11TreeB_casesyx_AA0C1BOyxG1lADyxG1rtlF
func TreeB_cases<T>(_ t: T, l: TreeB<T>, r: TreeB<T>) {

// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeB<T>.Type
// CHECK:         [[NIL:%.*]] = alloc_stack $TreeB<T>
// CHECK-NEXT:    inject_enum_addr [[NIL]] : $*TreeB<T>, #TreeB.Nil!enumelt
// CHECK-NEXT:    destroy_addr [[NIL]]
// CHECK-NEXT:    dealloc_stack [[NIL]]
  let _ = TreeB<T>.Nil

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeB<T>.Type
// CHECK-NEXT:    [[LEAF:%.*]] = alloc_stack $TreeB<T>
// CHECK-NEXT:    [[PAYLOAD:%.*]] = init_enum_data_addr [[LEAF]] : $*TreeB<T>, #TreeB.Leaf!enumelt.1
// CHECK-NEXT:    copy_addr %0 to [initialization] [[PAYLOAD]]
// CHECK-NEXT:    inject_enum_addr [[LEAF]] : $*TreeB<T>, #TreeB.Leaf!enumelt
// CHECK-NEXT:    destroy_addr [[LEAF]]
// CHECK-NEXT:    dealloc_stack [[LEAF]]
  let _ = TreeB<T>.Leaf(t)

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeB<T>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var τ_0_0 } <(left: TreeB<T>, right: TreeB<T>)>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    copy_addr %1 to [initialization] [[LEFT]] : $*TreeB<T>
// CHECK-NEXT:    copy_addr %2 to [initialization] [[RIGHT]] : $*TreeB<T>
// CHECK-NEXT:    [[BRANCH:%.*]] = alloc_stack $TreeB<T>
// CHECK-NEXT:    [[PAYLOAD:%.*]] = init_enum_data_addr [[BRANCH]]
// CHECK-NEXT:    store [[BOX]] to [init] [[PAYLOAD]]
// CHECK-NEXT:    inject_enum_addr [[BRANCH]] : $*TreeB<T>, #TreeB.Branch!enumelt.1
// CHECK-NEXT:    destroy_addr [[BRANCH]]
// CHECK-NEXT:    dealloc_stack [[BRANCH]]
// CHECK-NEXT:    destroy_addr %2
// CHECK-NEXT:    destroy_addr %1
// CHECK-NEXT:    destroy_addr %0
  let _ = TreeB<T>.Branch(left: l, right: r)

// CHECK:         return

}

// CHECK-LABEL: sil hidden @_T013indirect_enum13TreeInt_casesySi_AA0cD0O1lAD1rtF : $@convention(thin) (Int, @owned TreeInt, @owned TreeInt) -> ()
func TreeInt_cases(_ t: Int, l: TreeInt, r: TreeInt) {
// CHECK: bb0([[ARG1:%.*]] : $Int, [[ARG2:%.*]] : $TreeInt, [[ARG3:%.*]] : $TreeInt):
// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[NIL:%.*]] = enum $TreeInt, #TreeInt.Nil!enumelt
// CHECK-NEXT:    destroy_value [[NIL]]
  let _ = TreeInt.Nil

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeInt, #TreeInt.Leaf!enumelt.1, [[ARG1]]
// CHECK-NEXT:    destroy_value [[LEAF]]
  let _ = TreeInt.Leaf(t)

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var τ_0_0 } <(left: TreeInt, right: TreeInt)>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    [[BORROWED_ARG2:%.*]] = begin_borrow [[ARG2]]
// CHECK-NEXT:    [[ARG2_COPY:%.*]] = copy_value [[BORROWED_ARG2]]
// CHECK-NEXT:    store [[ARG2_COPY]] to [init] [[LEFT]]
// CHECK-NEXT:    [[BORROWED_ARG3:%.*]] = begin_borrow [[ARG3]]
// CHECK-NEXT:    [[ARG3_COPY:%.*]] = copy_value [[BORROWED_ARG3]]
// CHECK-NEXT:    store [[ARG3_COPY]] to [init] [[RIGHT]]
// CHECK-NEXT:    [[BRANCH:%.*]] = enum $TreeInt, #TreeInt.Branch!enumelt.1, [[BOX]]
// CHECK-NEXT:    destroy_value [[BRANCH]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG3]] from [[ARG3]]
// CHECK-NEXT:    end_borrow [[BORROWED_ARG2]] from [[ARG2]]
// CHECK-NEXT:    destroy_value [[ARG3]]
// CHECK-NEXT:    destroy_value [[ARG2]]
  let _ = TreeInt.Branch(left: l, right: r)
}
// CHECK: } // end sil function '_T013indirect_enum13TreeInt_casesySi_AA0cD0O1lAD1rtF'

enum TreeInt {
  case Nil
  case Leaf(Int)
  indirect case Branch(left: TreeInt, right: TreeInt)
}


enum TrivialButIndirect {
  case Direct(Int)
  indirect case Indirect(Int)
}

func a() {}
func b<T>(_ x: T) {}
func c<T>(_ x: T, _ y: T) {}
func d() {}

// CHECK-LABEL: sil hidden @_T013indirect_enum11switchTreeAyAA0D1AOyxGlF : $@convention(thin) <T> (@owned TreeA<T>) -> () {
func switchTreeA<T>(_ x: TreeA<T>) {
  // CHECK: bb0([[ARG:%.*]] : $TreeA<T>):
  // --           ref(x) == +1
  // CHECK:       [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:       [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // --           ref(x) == +2
  // CHECK:       switch_enum [[ARG_COPY]] : $TreeA<T>,
  // CHECK:          case #TreeA.Nil!enumelt: [[NIL_CASE:bb1]],
  // CHECK:          case #TreeA.Leaf!enumelt.1: [[LEAF_CASE:bb2]],
  // CHECK:          case #TreeA.Branch!enumelt.1: [[BRANCH_CASE:bb3]],
  switch x {
  // CHECK:     [[NIL_CASE]]:
  // CHECK:       end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:       function_ref @_T013indirect_enum1ayyF
  // CHECK:       br [[OUTER_CONT:bb[0-9]+]]
  case .Nil:
    a()
  // CHECK:     [[LEAF_CASE]]([[LEAF_BOX:%.*]] : $<τ_0_0> { var τ_0_0 } <T>):
  // CHECK:       [[VALUE:%.*]] = project_box [[LEAF_BOX]]
  // CHECK:       [[X:%.*]] = alloc_stack $T, let, name "x"
  // CHECK:       copy_addr [[VALUE]] to [initialization] [[X]] : $*T
  // --           ref(x) == +3
  // CHECK:       [[FUNC:%.*]] = function_ref @_T013indirect_enum1b{{[_0-9a-zA-Z]*}}F
  // CHECK:       [[TEMP:%.*]] = alloc_stack $T
  // CHECK:       copy_addr [[X]] to [initialization] [[TEMP]]
  // --           ref(x) == +4
  // CHECK:       apply [[FUNC]]<T>([[TEMP]]) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
  // --           ref(x) == +3
  // CHECK:       destroy_addr [[X]]
  // --           ref(x) == +2
  // CHECK:       dealloc_stack [[X]]
  // CHECK:       destroy_value [[LEAF_BOX]]
  // --           ref(x) == +1
  // CHECK:       end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:       br [[OUTER_CONT]]
  case .Leaf(let x):
    b(x)

  // CHECK:     [[BRANCH_CASE]]([[NODE_BOX:%.*]] : $<τ_0_0> { var τ_0_0 } <(left: TreeA<T>, right: TreeA<T>)>):
  // CHECK:       [[TUPLE_ADDR:%.*]] = project_box [[NODE_BOX]]
  // CHECK:       [[TUPLE:%.*]] = load_borrow [[TUPLE_ADDR]]
  // CHECK:       [[LEFT:%.*]] = tuple_extract [[TUPLE]] {{.*}}, 0
  // CHECK:       [[LEFT_COPY:%.*]] = copy_value [[LEFT]]
  // --           left = x[0]; ref(left) == +1
  // CHECK:       [[RIGHT:%.*]] = tuple_extract [[TUPLE]] {{.*}}, 1
  // CHECK:       [[RIGHT_COPY:%.*]] = copy_value [[RIGHT]]
  // --           right = x[0]; ref(right) == +1
  // ==> SEMANTIC SIL TODO: The borrow end should be here.
  // CHECK:       switch_enum [[RIGHT_COPY]] : $TreeA<T>,
  // CHECK:          case #TreeA.Leaf!enumelt.1: [[LEAF_CASE_RIGHT:bb[0-9]+]],
  // CHECK:          default [[FAIL_RIGHT:bb[0-9]+]]

  // CHECK:     [[LEAF_CASE_RIGHT]]([[RIGHT_LEAF_BOX:%.*]] : $<τ_0_0> { var τ_0_0 } <T>):
  // CHECK:       [[RIGHT_LEAF_VALUE:%.*]] = project_box [[RIGHT_LEAF_BOX]]
  // CHECK:       switch_enum [[LEFT_COPY]] : $TreeA<T>,
  // CHECK:          case #TreeA.Leaf!enumelt.1: [[LEAF_CASE_LEFT:bb[0-9]+]],
  // CHECK:          default [[FAIL_LEFT:bb[0-9]+]]
  
  // CHECK:     [[LEAF_CASE_LEFT]]([[LEFT_LEAF_BOX:%.*]] : $<τ_0_0> { var τ_0_0 } <T>):
  // CHECK:       [[LEFT_LEAF_VALUE:%.*]] = project_box [[LEFT_LEAF_BOX]]
  // CHECK:       [[LEFT_STACK:%.*]] = alloc_stack $T, let, name "x"  
  // CHECK:       copy_addr [[LEFT_LEAF_VALUE]] to [initialization] [[LEFT_STACK]]
  // --           ref(left) == +2
  // CHECK:       [[RIGHT_STACK:%.*]] = alloc_stack $T, let, name "y"
  // CHECK:       copy_addr [[RIGHT_LEAF_VALUE]] to [initialization] [[RIGHT_STACK]]
  // --           ref(right) == +2
  // CHECK:       [[LEFT_STACK_2:%.*]] = alloc_stack $T
  // CHECK:       copy_addr [[LEFT_STACK]] to [initialization] [[LEFT_STACK_2]]
  // --           ref(left) == +3
  // CHECK:       [[RIGHT_STACK_2:%.*]] = alloc_stack $T
  // CHECK:       copy_addr [[RIGHT_STACK]] to [initialization] [[RIGHT_STACK_2]]
  // --           ref(right) == +3
  // CHECK:       apply {{.*}}<T>([[LEFT_STACK_2]], [[RIGHT_STACK_2]]) : $@convention(thin) <τ_0_0> (@in τ_0_0, @in τ_0_0) -> ()
  // --           ref(left) == +2, ref(right) == +2
  // CHECK:       destroy_addr [[RIGHT_STACK]]
  // --           ref(right) == +1
  // CHECK:       destroy_addr [[LEFT_STACK]]
  // --           ref(left) == +1
  // CHECK:       destroy_value [[LEFT_LEAF_BOX]]
  // --           ref(left) == +0
  // CHECK:       destroy_value [[RIGHT_LEAF_BOX]]
  // --           ref(right) == +0
  // CHECK:       destroy_value [[NODE_BOX]]
  // --           ref(x) == +1
  // CHECK:       end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:       br [[OUTER_CONT]]

  // CHECK:     [[FAIL_LEFT]]:
  // CHECK:       br [[DEFAULT:bb[0-9]+]]

  // CHECK:     [[FAIL_RIGHT]]:
  // CHECK:       br [[DEFAULT]]

  case .Branch(.Leaf(let x), .Leaf(let y)):
    c(x, y)

  // CHECK:     [[DEFAULT]]:
  // --           x +1
  // CHECK:       destroy_value [[ARG_COPY]]
  // CHECK:       end_borrow [[BORROWED_ARG]] from [[ARG]]
  default:
    d()
  }

  // CHECK:     [[OUTER_CONT:%.*]]:
  // --           ref(x) == +0
  // CHECK:       destroy_value [[ARG]] : $TreeA<T>
}
// CHECK: } // end sil function '_T013indirect_enum11switchTreeAyAA0D1AOyxGlF'

// CHECK-LABEL: sil hidden @_T013indirect_enum11switchTreeB{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T> (@in TreeB<T>) -> ()
func switchTreeB<T>(_ x: TreeB<T>) {
  // CHECK: bb0([[X:%.*]] : $*TreeB<T>):
  // --       ref(x) == +1
  // CHECK:   [[SCRATCH:%.*]] = alloc_stack $TreeB<T>
  // CHECK:   copy_addr [[X]] to [initialization] [[SCRATCH]] : $*TreeB<T>
  // --       ref(x) == +2
  // CHECK:   switch_enum_addr [[SCRATCH]] : $*TreeB<T>, case #TreeB.Nil!enumelt: [[NIL_BB:bb[0-9]+]], case #TreeB.Leaf!enumelt.1: [[LEAF_BB:bb[0-9]+]], case #TreeB.Branch!enumelt.1: [[BRANCH_BB:bb[0-9]+]]
  switch x {

  // CHECK: [[NIL_BB]]:
  // ==> SEMANTIC SIL TODO: This next destroy_addr is unnecessary.
  // CHECK:   destroy_addr [[SCRATCH]]
  // CHECK:   dealloc_stack [[SCRATCH]]
  // CHECK:   function_ref @_T013indirect_enum1ayyF
  // CHECK:   br [[OUTER_CONT:bb[0-9]+]]
  case .Nil:
    a()

  // CHECK:   [[LEAF_BB]]:
  // CHECK:     copy_addr [[SCRATCH]] to [initialization] [[LEAF_COPY:%.*]] :
  // CHECK:     [[LEAF_ADDR:%.*]] = unchecked_take_enum_data_addr [[LEAF_COPY]]
  // CHECK:     copy_addr [take] [[LEAF_ADDR]] to [initialization] [[LEAF:%.*]] :
  // CHECK:     function_ref @_T013indirect_enum1b{{[_0-9a-zA-Z]*}}F
  // CHECK:     destroy_addr [[LEAF]]
  // CHECK:     dealloc_stack [[LEAF]]
  // CHECK-NOT: destroy_addr [[LEAF_COPY]]
  // CHECK:     dealloc_stack [[LEAF_COPY]]
  // CHECK:     destroy_addr [[SCRATCH]]
  // CHECK:     dealloc_stack [[SCRATCH]]
  // CHECK:     br [[OUTER_CONT]]
  case .Leaf(let x):
    b(x)

  // CHECK:   [[BRANCH_BB]]:
  // CHECK:       copy_addr [[SCRATCH]] to [initialization] [[TREE_COPY:%.*]] :
  // CHECK:       [[TREE_ADDR:%.*]] = unchecked_take_enum_data_addr [[TREE_COPY]]
  // --           box +1 immutable
  // CHECK:       [[BOX:%.*]] = load [take] [[TREE_ADDR]]
  // CHECK:       [[TUPLE:%.*]] = project_box [[BOX]]
  // CHECK:       [[LEFT:%.*]] = tuple_element_addr [[TUPLE]]
  // CHECK:       [[LEFT_COPY:%.*]] = alloc_stack $TreeB<T>
  // CHECK:       copy_addr [[LEFT]] to [initialization] [[LEFT_COPY]] 
  // CHECK:       [[RIGHT:%.*]] = tuple_element_addr [[TUPLE]]
  // CHECK:       [[RIGHT_COPY:%.*]] = alloc_stack $TreeB<T>
  // CHECK:       copy_addr [[RIGHT]] to [initialization] [[RIGHT_COPY]] 
  // CHECK:       switch_enum_addr [[RIGHT_COPY]] : $*TreeB<T>, case #TreeB.Leaf!enumelt.1: [[RIGHT_SUCCESS:bb[0-9]+]], default [[RIGHT_FAIL:bb[0-9]+]]

  // CHECK:     [[RIGHT_SUCCESS]]:
  // CHECK:       [[RIGHT_LEAF:%.*]] = unchecked_take_enum_data_addr [[RIGHT_COPY]] : $*TreeB<T>, #TreeB.Leaf
  // CHECK:       switch_enum_addr [[LEFT_COPY]] : $*TreeB<T>, case #TreeB.Leaf!enumelt.1: [[LEFT_SUCCESS:bb[0-9]+]], default [[LEFT_FAIL:bb[0-9]+]]

  // CHECK:     [[LEFT_SUCCESS]]:
  // CHECK-NEXT:       [[LEFT_LEAF:%.*]] = unchecked_take_enum_data_addr [[LEFT_COPY]] : $*TreeB<T>, #TreeB.Leaf
  // CHECK:       [[LEFT_COPY_COPY:%.*]] = alloc_stack $T, let, name "x"
  // CHECK:       copy_addr [take] [[LEFT_LEAF]] to [initialization] [[LEFT_COPY_COPY]]
  // CHECK:       [[RIGHT_COPY_COPY:%.*]] = alloc_stack $T, let, name "y"
  // CHECK:       copy_addr [take] [[RIGHT_LEAF]] to [initialization] [[RIGHT_COPY_COPY]]
  // CHECK:       [[FUNC:%.*]] = function_ref @_T013indirect_enum1c{{[_0-9a-zA-Z]*}}F
  // CHECK:       [[LEFT_ARG_COPY:%.*]] = alloc_stack $T
  // CHECK:       copy_addr [[LEFT_COPY_COPY]] to [initialization] [[LEFT_ARG_COPY]]
  // CHECK:       [[RIGHT_ARG_COPY:%.*]] = alloc_stack $T
  // CHECK:       copy_addr [[RIGHT_COPY_COPY]] to [initialization] [[RIGHT_ARG_COPY]]
  // CHECK:       apply [[FUNC]]<T>([[LEFT_ARG_COPY]], [[RIGHT_ARG_COPY]])
  // CHECK-NEXT:       dealloc_stack [[RIGHT_ARG_COPY]]
  // CHECK-NEXT:       dealloc_stack [[LEFT_ARG_COPY]]
  // CHECK-NEXT:       destroy_addr [[RIGHT_COPY_COPY]]
  // CHECK-NEXT:       dealloc_stack [[RIGHT_COPY_COPY]]
  // CHECK-NEXT:       destroy_addr [[LEFT_COPY_COPY]]
  // CHECK-NEXT:       dealloc_stack [[LEFT_COPY_COPY]]
  // CHECK-NEXT:       dealloc_stack [[RIGHT_COPY]]
  // CHECK-NEXT:       dealloc_stack [[LEFT_COPY]]
  // --           box +0
  // CHECK-NEXT:       destroy_value [[BOX]]
  // CHECK-NEXT:       dealloc_stack [[TREE_COPY]]
  // CHECK-NEXT:       destroy_addr [[SCRATCH]]
  // CHECK-NEXT:       dealloc_stack [[SCRATCH]]
  // CHECK-NEXT:       br [[OUTER_CONT]]
  case .Branch(.Leaf(let x), .Leaf(let y)):
    c(x, y)

  // CHECK:     [[LEFT_FAIL]]:
  // CHECK:       destroy_addr [[RIGHT_LEAF]]
  // CHECK-NOT:   destroy_addr [[RIGHT_COPY]]
  // CHECK:       dealloc_stack [[RIGHT_COPY]]
  // CHECK:       destroy_value [[BOX]]
  // CHECK-NOT:   destroy_addr [[TREE_COPY]]
  // CHECK:       dealloc_stack [[TREE_COPY]]
  // CHECK:       br [[INNER_CONT:bb[0-9]+]]

  // CHECK:     [[RIGHT_FAIL]]:
  // CHECK:       destroy_value [[BOX]]
  // CHECK-NOT:   destroy_addr [[TREE_COPY]]
  // CHECK:       dealloc_stack [[TREE_COPY]]
  // CHECK:       br [[INNER_CONT:bb[0-9]+]]

  // CHECK:     [[INNER_CONT]]:
  // CHECK:       destroy_addr [[SCRATCH]]
  // CHECK:       dealloc_stack [[SCRATCH]]
  // CHECK:       function_ref @_T013indirect_enum1dyyF
  // CHECK:       br [[OUTER_CONT]]
  default:
    d()
  }
  // CHECK:     [[OUTER_CONT]]:
  // CHECK:       destroy_addr [[X]]
}

// CHECK-LABEL: sil hidden @_T013indirect_enum10guardTreeA{{[_0-9a-zA-Z]*}}F
func guardTreeA<T>(_ tree: TreeA<T>) {
  // CHECK: bb0([[ARG:%.*]] : $TreeA<T>):
  do {
    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK: [[YES]]:
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    guard case .Nil = tree else { return }

    // CHECK:   [[X:%.*]] = alloc_stack $T
    // CHECK:   [[BORROWED_ARG_2:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG_2]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Leaf!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG_2]] from [[ARG]]
    // CHECK: [[YES]]([[BOX:%.*]] : $<τ_0_0> { var τ_0_0 } <T>):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TMP:%.*]] = alloc_stack
    // CHECK:   copy_addr [[VALUE_ADDR]] to [initialization] [[TMP]]
    // CHECK:   copy_addr [take] [[TMP]] to [initialization] [[X]]
    // CHECK:   destroy_value [[BOX]]
    guard case .Leaf(let x) = tree else { return }

    // CHECK:   [[BORROWED_ARG_3:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG_3]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Branch!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG_3]] from [[ARG]]
    // CHECK: [[YES]]([[BOX:%.*]] : $<τ_0_0> { var τ_0_0 } <(left: TreeA<T>, right: TreeA<T>)>):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TUPLE:%.*]] = load [take] [[VALUE_ADDR]]
    // CHECK:   [[TUPLE_COPY:%.*]] = copy_value [[TUPLE]]
    // CHECK:   [[L:%.*]] = tuple_extract [[TUPLE_COPY]]
    // CHECK:   [[R:%.*]] = tuple_extract [[TUPLE_COPY]]
    // CHECK:   destroy_value [[BOX]]
    // CHECK:   end_borrow [[BORROWED_ARG_3]] from [[ARG]]
    guard case .Branch(left: let l, right: let r) = tree else { return }

    // CHECK:   destroy_value [[R]]
    // CHECK:   destroy_value [[L]]
    // CHECK:   destroy_addr [[X]]
  }

  do {
    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK: [[YES]]:
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    if case .Nil = tree { }

    // CHECK:   [[X:%.*]] = alloc_stack $T
    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Leaf!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK: [[YES]]([[BOX:%.*]] : $<τ_0_0> { var τ_0_0 } <T>):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TMP:%.*]] = alloc_stack
    // CHECK:   copy_addr [[VALUE_ADDR]] to [initialization] [[TMP]]
    // CHECK:   copy_addr [take] [[TMP]] to [initialization] [[X]]
    // CHECK:   destroy_value [[BOX]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK:   destroy_addr [[X]]
    if case .Leaf(let x) = tree { }


    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Branch!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK: [[YES]]([[BOX:%.*]] : $<τ_0_0> { var τ_0_0 } <(left: TreeA<T>, right: TreeA<T>)>):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TUPLE:%.*]] = load [take] [[VALUE_ADDR]]
    // CHECK:   [[TUPLE_COPY:%.*]] = copy_value [[TUPLE]]
    // CHECK:   [[L:%.*]] = tuple_extract [[TUPLE_COPY]]
    // CHECK:   [[R:%.*]] = tuple_extract [[TUPLE_COPY]]
    // CHECK:   destroy_value [[BOX]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK:   destroy_value [[R]]
    // CHECK:   destroy_value [[L]]
    if case .Branch(left: let l, right: let r) = tree { }
  }
}

// CHECK-LABEL: sil hidden @_T013indirect_enum10guardTreeB{{[_0-9a-zA-Z]*}}F
func guardTreeB<T>(_ tree: TreeB<T>) {
  do {
    // CHECK:   copy_addr %0 to [initialization] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   destroy_addr [[TMP]]
    guard case .Nil = tree else { return }

    // CHECK:   [[X:%.*]] = alloc_stack $T
    // CHECK:   copy_addr %0 to [initialization] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Leaf!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   [[VALUE:%.*]] = unchecked_take_enum_data_addr [[TMP]]
    // CHECK:   copy_addr [take] [[VALUE]] to [initialization] [[X]]
    // CHECK:   dealloc_stack [[TMP]]
    guard case .Leaf(let x) = tree else { return }

    // CHECK:   [[L:%.*]] = alloc_stack $TreeB
    // CHECK:   [[R:%.*]] = alloc_stack $TreeB
    // CHECK:   copy_addr %0 to [initialization] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Branch!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   [[BOX_ADDR:%.*]] = unchecked_take_enum_data_addr [[TMP]]
    // CHECK:   [[BOX:%.*]] = load [take] [[BOX_ADDR]]
    // CHECK:   [[TUPLE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   copy_addr [[TUPLE_ADDR]] to [initialization] [[TUPLE_COPY:%.*]] :
    // CHECK:   [[L_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[L_COPY]] to [initialization] [[L]]
    // CHECK:   [[R_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[R_COPY]] to [initialization] [[R]]
    // CHECK:   destroy_value [[BOX]]
    guard case .Branch(left: let l, right: let r) = tree else { return }

    // CHECK:   destroy_addr [[R]]
    // CHECK:   destroy_addr [[L]]
    // CHECK:   destroy_addr [[X]]
  }

  do {
    // CHECK:   copy_addr %0 to [initialization] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   destroy_addr [[TMP]]
    if case .Nil = tree { }

    // CHECK:   [[X:%.*]] = alloc_stack $T
    // CHECK:   copy_addr %0 to [initialization] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Leaf!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   [[VALUE:%.*]] = unchecked_take_enum_data_addr [[TMP]]
    // CHECK:   copy_addr [take] [[VALUE]] to [initialization] [[X]]
    // CHECK:   dealloc_stack [[TMP]]
    // CHECK:   destroy_addr [[X]]
    if case .Leaf(let x) = tree { }

    // CHECK:   [[L:%.*]] = alloc_stack $TreeB
    // CHECK:   [[R:%.*]] = alloc_stack $TreeB
    // CHECK:   copy_addr %0 to [initialization] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Branch!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   [[BOX_ADDR:%.*]] = unchecked_take_enum_data_addr [[TMP]]
    // CHECK:   [[BOX:%.*]] = load [take] [[BOX_ADDR]]
    // CHECK:   [[TUPLE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   copy_addr [[TUPLE_ADDR]] to [initialization] [[TUPLE_COPY:%.*]] :
    // CHECK:   [[L_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[L_COPY]] to [initialization] [[L]]
    // CHECK:   [[R_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[R_COPY]] to [initialization] [[R]]
    // CHECK:   destroy_value [[BOX]]
    // CHECK:   destroy_addr [[R]]
    // CHECK:   destroy_addr [[L]]
    if case .Branch(left: let l, right: let r) = tree { }
  }
}

// SEMANTIC ARC TODO: This test needs to be made far more comprehensive.
// CHECK-LABEL: sil hidden @_T013indirect_enum35dontDisableCleanupOfIndirectPayloadyAA010TrivialButG0OF : $@convention(thin) (@owned TrivialButIndirect) -> () {
func dontDisableCleanupOfIndirectPayload(_ x: TrivialButIndirect) {
  // CHECK: bb0([[ARG:%.*]] : $TrivialButIndirect):
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:   switch_enum [[ARG_COPY]] : $TrivialButIndirect, case #TrivialButIndirect.Direct!enumelt.1:  [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
  // CHECK: [[NO]]:
  // CHECK:   destroy_value [[ARG_COPY]]
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  guard case .Direct(let foo) = x else { return }

  // -- Cleanup isn't necessary on "no" path because .Direct is trivial
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK:   switch_enum [[ARG_COPY]] : $TrivialButIndirect, case #TrivialButIndirect.Indirect!enumelt.1:  [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]

  // => SEMANTIC SIL TODO: This test case needs to be extended below
  // CHECK: [[NO]]:
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]

  // CHECK: [[YES]]({{.*}}):

  guard case .Indirect(let bar) = x else { return }
}
// CHECK: } // end sil function '_T013indirect_enum35dontDisableCleanupOfIndirectPayloadyAA010TrivialButG0OF'
