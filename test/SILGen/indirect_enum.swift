
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name indirect_enum -Xllvm -sil-print-debuginfo %s | %FileCheck %s

indirect enum TreeA<T> {
  case Nil
  case Leaf(T)
  case Branch(left: TreeA<T>, right: TreeA<T>)
}

// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum11TreeA_cases_1l1ryx_AA0C1AOyxGAGtlF : $@convention(thin) <T> (@in_guaranteed T, @guaranteed TreeA<T>, @guaranteed TreeA<T>) -> () {
func TreeA_cases<T>(_ t: T, l: TreeA<T>, r: TreeA<T>) {
// CHECK: bb0([[ARG1:%.*]] : $*T, [[ARG2:%.*]] : @guaranteed $TreeA<T>, [[ARG3:%.*]] : @guaranteed $TreeA<T>):
// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[NIL:%.*]] = enum $TreeA<T>, #TreeA.Nil!enumelt
// CHECK-NOT:     destroy_value [[NIL]]
  let _ = TreeA<T>.Nil

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var τ_0_0 } <T>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    copy_addr [[ARG1]] to [init] [[PB]]
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeA<T>, #TreeA.Leaf!enumelt, [[BOX]]
// CHECK-NEXT:    destroy_value [[LEAF]]
  let _ = TreeA<T>.Leaf(t)

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var (left: TreeA<τ_0_0>, right: TreeA<τ_0_0>) } <T>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]] : $*(left: TreeA<T>, right: TreeA<T>), 0
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]] : $*(left: TreeA<T>, right: TreeA<T>), 1
// CHECK-NEXT:    [[ARG2_COPY:%.*]] = copy_value [[ARG2]]
// CHECK-NEXT:    store [[ARG2_COPY]] to [init] [[LEFT]]
// CHECK-NEXT:    [[ARG3_COPY:%.*]] = copy_value [[ARG3]]
// CHECK-NEXT:    store [[ARG3_COPY]] to [init] [[RIGHT]]
// CHECK-NEXT:    [[BRANCH:%.*]] = enum $TreeA<T>, #TreeA.Branch!enumelt, [[BOX]]
// CHECK-NEXT:    destroy_value [[BRANCH]]
  let _ = TreeA<T>.Branch(left: l, right: r)

}
// CHECK: // end sil function '$s13indirect_enum11TreeA_cases_1l1ryx_AA0C1AOyxGAGtlF'

// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum16TreeA_reabstractyyS2icF : $@convention(thin) (@guaranteed @callee_guaranteed (Int) -> Int) -> () {
func TreeA_reabstract(_ f: @escaping (Int) -> Int) {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@callee_guaranteed (Int) -> Int):
// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeA<(Int) -> Int>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var τ_0_0 } <(Int) -> Int>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:         [[THUNK:%.*]] = function_ref @$sS2iIegyd_S2iIegnr_TR
// CHECK-NEXT:    [[FN:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[ARG_COPY]])
// CHECK-NEXT:    [[FNC:%.*]] = convert_function [[FN]]
// CHECK-NEXT:    store [[FNC]] to [init] [[PB]]
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeA<(Int) -> Int>, #TreeA.Leaf!enumelt, [[BOX]]
// CHECK-NEXT:    destroy_value [[LEAF]]
// CHECK: return
  let _ = TreeA<(Int) -> Int>.Leaf(f)
}
// CHECK: } // end sil function '$s13indirect_enum16TreeA_reabstractyyS2icF'

enum TreeB<T> {
  case Nil
  case Leaf(T)
  indirect case Branch(left: TreeB<T>, right: TreeB<T>)
}

// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum11TreeB_cases_1l1ryx_AA0C1BOyxGAGtlF
func TreeB_cases<T>(_ t: T, l: TreeB<T>, r: TreeB<T>) {

// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeB<T>.Type
// CHECK:         [[NIL:%.*]] = alloc_stack $TreeB<T>
// CHECK-NEXT:    inject_enum_addr [[NIL]] : $*TreeB<T>, #TreeB.Nil!enumelt
// CHECK-NEXT:    destroy_addr [[NIL]]
// CHECK-NEXT:    dealloc_stack [[NIL]]
  let _ = TreeB<T>.Nil

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeB<T>.Type
// CHECK-NEXT:    [[LEAF:%.*]] = alloc_stack $TreeB<T>
// CHECK-NEXT:    [[PAYLOAD:%.*]] = init_enum_data_addr [[LEAF]] : $*TreeB<T>, #TreeB.Leaf!enumelt
// CHECK-NEXT:    copy_addr %0 to [init] [[PAYLOAD]]
// CHECK-NEXT:    inject_enum_addr [[LEAF]] : $*TreeB<T>, #TreeB.Leaf!enumelt
// CHECK-NEXT:    destroy_addr [[LEAF]]
// CHECK-NEXT:    dealloc_stack [[LEAF]]
  let _ = TreeB<T>.Leaf(t)

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeB<T>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $<τ_0_0> { var (left: TreeB<τ_0_0>, right: TreeB<τ_0_0>) } <T>
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    copy_addr %1 to [init] [[LEFT]] : $*TreeB<T>
// CHECK-NEXT:    copy_addr %2 to [init] [[RIGHT]] : $*TreeB<T>
// CHECK-NEXT:    [[BRANCH:%.*]] = alloc_stack $TreeB<T>
// CHECK-NEXT:    [[PAYLOAD:%.*]] = init_enum_data_addr [[BRANCH]]
// CHECK-NEXT:    store [[BOX]] to [init] [[PAYLOAD]]
// CHECK-NEXT:    inject_enum_addr [[BRANCH]] : $*TreeB<T>, #TreeB.Branch!enumelt
// CHECK-NEXT:    destroy_addr [[BRANCH]]
// CHECK-NEXT:    dealloc_stack [[BRANCH]]
  let _ = TreeB<T>.Branch(left: l, right: r)

// CHECK:         return
}

// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum13TreeInt_cases_1l1rySi_AA0cD0OAFtF : $@convention(thin) (Int, @guaranteed TreeInt, @guaranteed TreeInt) -> ()
func TreeInt_cases(_ t: Int, l: TreeInt, r: TreeInt) {
// CHECK: bb0([[ARG1:%.*]] : $Int, [[ARG2:%.*]] : @guaranteed $TreeInt, [[ARG3:%.*]] : @guaranteed $TreeInt):
// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[NIL:%.*]] = enum $TreeInt, #TreeInt.Nil!enumelt
// CHECK-NOT:     destroy_value [[NIL]]
  let _ = TreeInt.Nil

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeInt, #TreeInt.Leaf!enumelt, [[ARG1]]
// CHECK-NOT:     destroy_value [[LEAF]]
  let _ = TreeInt.Leaf(t)

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box ${ var (left: TreeInt, right: TreeInt) }
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    [[ARG2_COPY:%.*]] = copy_value [[ARG2]] : $TreeInt
// CHECK-NEXT:    store [[ARG2_COPY]] to [init] [[LEFT]]
// CHECK-NEXT:    [[ARG3_COPY:%.*]] = copy_value [[ARG3]] : $TreeInt
// CHECK-NEXT:    store [[ARG3_COPY]] to [init] [[RIGHT]]
// CHECK-NEXT:    [[BRANCH:%.*]] = enum $TreeInt, #TreeInt.Branch!enumelt, [[BOX]]
// CHECK-NEXT:    destroy_value [[BRANCH]]
  let _ = TreeInt.Branch(left: l, right: r)
}
// CHECK: } // end sil function '$s13indirect_enum13TreeInt_cases_1l1rySi_AA0cD0OAFtF'

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

// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum11switchTreeAyyAA0D1AOyxGlF : $@convention(thin) <T> (@guaranteed TreeA<T>) -> () {
func switchTreeA<T>(_ x: TreeA<T>) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $TreeA<T>):
  // --           x +0
  // CHECK:       switch_enum [[ARG]] : $TreeA<T>,
  // CHECK:          case #TreeA.Nil!enumelt: [[NIL_CASE:bb1]],
  // CHECK:          case #TreeA.Leaf!enumelt: [[LEAF_CASE:bb2]],
  // CHECK:          case #TreeA.Branch!enumelt: [[BRANCH_CASE:bb3]],
  switch x {
  // CHECK:     [[NIL_CASE]]:
  // CHECK:       function_ref @$s13indirect_enum1ayyF
  // CHECK:       br [[OUTER_CONT:bb[0-9]+]]
  case .Nil:
    a()
  // CHECK:     [[LEAF_CASE]]([[LEAF_BOX:%.*]] : @guaranteed $<τ_0_0> { var τ_0_0 } <T>):
  // CHECK:       [[VALUE:%.*]] = project_box [[LEAF_BOX]]
  // CHECK:       copy_addr [[VALUE]] to [init] [[X:%.*]] : $*T
  // CHECK:       function_ref @$s13indirect_enum1b{{[_0-9a-zA-Z]*}}F
  // CHECK:       destroy_addr [[X]]
  // CHECK:       dealloc_stack [[X]]
  // --           x +0
  // CHECK:       br [[OUTER_CONT]]
  case .Leaf(let x):
    b(x)

  // CHECK:     [[BRANCH_CASE]]([[NODE_BOX:%.*]] : @guaranteed $<τ_0_0> { var (left: TreeA<τ_0_0>, right: TreeA<τ_0_0>) } <T>):
  // CHECK:       [[TUPLE_ADDR:%.*]] = project_box [[NODE_BOX]]
  // CHECK:       [[TUPLE:%.*]] = load_borrow [[TUPLE_ADDR]]
  // CHECK:       ([[LEFT:%.*]], [[RIGHT:%.*]]) = destructure_tuple [[TUPLE]]
  // CHECK:       switch_enum [[LEFT]] : $TreeA<T>,
  // CHECK:          case #TreeA.Leaf!enumelt: [[LEAF_CASE_LEFT:bb[0-9]+]],
  // CHECK:          default [[FAIL_LEFT:bb[0-9]+]]

  // CHECK:     [[LEAF_CASE_LEFT]]([[LEFT_LEAF_BOX:%.*]] : @guaranteed $<τ_0_0> { var τ_0_0 } <T>):
  // CHECK:       [[LEFT_LEAF_VALUE:%.*]] = project_box [[LEFT_LEAF_BOX]]
  // CHECK:       switch_enum [[RIGHT]] : $TreeA<T>,
  // CHECK:          case #TreeA.Leaf!enumelt: [[LEAF_CASE_RIGHT:bb[0-9]+]],
  // CHECK:          default [[FAIL_RIGHT:bb[0-9]+]]

  // CHECK:     [[LEAF_CASE_RIGHT]]([[RIGHT_LEAF_BOX:%.*]] : @guaranteed $<τ_0_0> { var τ_0_0 } <T>):
  // CHECK:       [[RIGHT_LEAF_VALUE:%.*]] = project_box [[RIGHT_LEAF_BOX]]
  // CHECK:       copy_addr [[LEFT_LEAF_VALUE]]
  // CHECK:       copy_addr [[RIGHT_LEAF_VALUE]]
  // --           x +1
  // CHECK:       br [[OUTER_CONT]]

  // CHECK:     [[FAIL_RIGHT]]([[DEFAULT_VAL:%.*]] : @guaranteed
  // CHECK:       br [[DEFAULT:bb[0-9]+]]

  // CHECK:     [[FAIL_LEFT]]([[DEFAULT_VAL:%.*]] : @guaranteed
  // CHECK:       br [[DEFAULT]]

  case .Branch(.Leaf(let x), .Leaf(let y)):
    c(x, y)

  // CHECK:     [[DEFAULT]]:
  // --           x +0
  default:
    d()
  }

  // CHECK:     [[OUTER_CONT:%.*]]:
  // --           x +0
}
// CHECK: } // end sil function '$s13indirect_enum11switchTreeAyyAA0D1AOyxGlF'

// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum11switchTreeB{{[_0-9a-zA-Z]*}}F
func switchTreeB<T>(_ x: TreeB<T>) {
  // CHECK:       copy_addr %0 to [init] [[SCRATCH:%.*]] :
  // CHECK:       switch_enum_addr [[SCRATCH]]
  switch x {

  // CHECK:     bb{{.*}}:
  // CHECK:       function_ref @$s13indirect_enum1ayyF
  // CHECK:       destroy_addr [[SCRATCH]]
  // CHECK:       dealloc_stack [[SCRATCH]]
  // CHECK:       br [[OUTER_CONT:bb[0-9]+]]
  case .Nil:
    a()

  // CHECK:     bb{{.*}}:
  // CHECK:       copy_addr [[SCRATCH]] to [init] [[LEAF_COPY:%.*]] :
  // CHECK:       [[LEAF_ADDR:%.*]] = unchecked_take_enum_data_addr [[LEAF_COPY]]
  // CHECK:       copy_addr [take] [[LEAF_ADDR]] to [init] [[LEAF:%.*]] :
  // CHECK:       function_ref @$s13indirect_enum1b{{[_0-9a-zA-Z]*}}F
  // CHECK:       destroy_addr [[LEAF]]
  // CHECK:       dealloc_stack [[LEAF]]
  // CHECK-NOT:   destroy_addr [[LEAF_COPY]]
  // CHECK:       dealloc_stack [[LEAF_COPY]]
  // CHECK:       destroy_addr [[SCRATCH]]
  // CHECK:       dealloc_stack [[SCRATCH]]
  // CHECK:       br [[OUTER_CONT]]
  case .Leaf(let x):
    b(x)

  // CHECK:     bb{{.*}}:
  // CHECK:       copy_addr [[SCRATCH]] to [init] [[TREE_COPY:%.*]] :
  // CHECK:       [[TREE_ADDR:%.*]] = unchecked_take_enum_data_addr [[TREE_COPY]]
  // --           box +1 immutable
  // CHECK:       [[BOX:%.*]] = load [take] [[TREE_ADDR]]
  // CHECK:       [[TUPLE:%.*]] = project_box [[BOX]]
  // CHECK:       [[LEFT:%.*]] = tuple_element_addr [[TUPLE]]
  // CHECK:       [[RIGHT:%.*]] = tuple_element_addr [[TUPLE]]
  // CHECK:       switch_enum_addr [[LEFT]] {{.*}}, default [[LEFT_FAIL:bb[0-9]+]]

  // CHECK:     bb{{.*}}:
  // CHECK:       copy_addr [[LEFT]] to [init] [[LEFT_COPY:%.*]] :
  // CHECK:       [[LEFT_LEAF:%.*]] = unchecked_take_enum_data_addr [[LEFT_COPY]] : $*TreeB<T>, #TreeB.Leaf
  // CHECK:       switch_enum_addr [[RIGHT]] {{.*}}, default [[RIGHT_FAIL:bb[0-9]+]]

  // CHECK:     bb{{.*}}:
  // CHECK:       copy_addr [[RIGHT]] to [init] [[RIGHT_COPY:%.*]] :
  // CHECK:       [[RIGHT_LEAF:%.*]] = unchecked_take_enum_data_addr [[RIGHT_COPY]] : $*TreeB<T>, #TreeB.Leaf
  // CHECK:       copy_addr [take] [[LEFT_LEAF]] to [init] [[X:%.*]] :
  // CHECK:       copy_addr [take] [[RIGHT_LEAF]] to [init] [[Y:%.*]] :
  // CHECK:       function_ref @$s13indirect_enum1c{{[_0-9a-zA-Z]*}}F
  // CHECK:       destroy_addr [[Y]]
  // CHECK:       dealloc_stack [[Y]]
  // CHECK:       destroy_addr [[X]]
  // CHECK:       dealloc_stack [[X]]
  // CHECK-NOT:   destroy_addr [[RIGHT_COPY]]
  // CHECK:       dealloc_stack [[RIGHT_COPY]]
  // CHECK-NOT:   destroy_addr [[LEFT_COPY]]
  // CHECK:       dealloc_stack [[LEFT_COPY]]
  // --           box +0
  // CHECK:       destroy_value [[BOX]]
  // CHECK-NOT:   destroy_addr [[TREE_COPY]]
  // CHECK:       dealloc_stack [[TREE_COPY]]
  // CHECK:       destroy_addr [[SCRATCH]]
  // CHECK:       dealloc_stack [[SCRATCH]]
  case .Branch(.Leaf(let x), .Leaf(let y)):
    c(x, y)

  // CHECK:     [[RIGHT_FAIL]]:
  // CHECK:       destroy_addr [[LEFT_LEAF]]
  // CHECK-NOT:   destroy_addr [[LEFT_COPY]]
  // CHECK:       dealloc_stack [[LEFT_COPY]]
  // CHECK:       destroy_value [[BOX]]
  // CHECK-NOT:   destroy_addr [[TREE_COPY]]
  // CHECK:       dealloc_stack [[TREE_COPY]]
  // CHECK:       br [[INNER_CONT:bb[0-9]+]]

  // CHECK:     [[LEFT_FAIL]]:
  // CHECK:       destroy_value [[BOX]]
  // CHECK-NOT:   destroy_addr [[TREE_COPY]]
  // CHECK:       dealloc_stack [[TREE_COPY]]
  // CHECK:       br [[INNER_CONT:bb[0-9]+]]

  // CHECK:     [[INNER_CONT]]:
  // CHECK:       function_ref @$s13indirect_enum1dyyF
  // CHECK:       destroy_addr [[SCRATCH]]
  // CHECK:       dealloc_stack [[SCRATCH]]
  // CHECK:       br [[OUTER_CONT]]
  default:
    d()
  }
  // CHECK:     [[OUTER_CONT]]:
  // CHECK:       return
}

// Make sure that switchTreeInt obeys ownership invariants.
//
// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum13switchTreeInt{{[_0-9a-zA-Z]*}}F
func switchTreeInt(_ x: TreeInt) {
  switch x {

  case .Nil:
    a()

  case .Leaf(let x):
    b(x)

  case .Branch(.Leaf(let x), .Leaf(let y)):
    c(x, y)

  default:
    d()
  }
}
// CHECK: } // end sil function '$s13indirect_enum13switchTreeInt{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum10guardTreeA{{[_0-9a-zA-Z]*}}F
func guardTreeA<T>(_ tree: TreeA<T>) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $TreeA<T>):
  do {
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO1:bb[0-9]+]]
    // CHECK: [[YES]]:
    guard case .Nil = tree else { return }

    // CHECK:   [[X:%.*]] = alloc_stack [lexical] [var_decl] $T
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Leaf!enumelt: [[YES:bb[0-9]+]], default [[NO2:bb[0-9]+]]
    // CHECK: [[YES]]([[BOX:%.*]] : @owned $<τ_0_0> { var τ_0_0 } <T>):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TMP:%.*]] = alloc_stack
    // CHECK:   copy_addr [[VALUE_ADDR]] to [init] [[TMP]]
    // CHECK:   copy_addr [take] [[TMP]] to [init] [[X]]
    // CHECK:   destroy_value [[BOX]]
    guard case .Leaf(let x) = tree else { return }

    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Branch!enumelt: [[YES:bb[0-9]+]], default [[NO3:bb[0-9]+]]
    // CHECK: [[YES]]([[BOX:%.*]] : @owned $<τ_0_0> { var (left: TreeA<τ_0_0>, right: TreeA<τ_0_0>) } <T>):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TUPLE:%.*]] = load_borrow [[VALUE_ADDR]]
    // CHECK:   [[TUPLE_COPY:%.*]] = copy_value [[TUPLE]]
    // CHECK:   end_borrow [[TUPLE]]
    // CHECK:   ([[L:%.*]], [[R:%.*]]) = destructure_tuple [[TUPLE_COPY]]
    // CHECK:   [[MOVED_L:%.*]] = move_value [lexical] [var_decl] [[L]]
    // CHECK:   [[MOVED_R:%.*]] = move_value [lexical] [var_decl] [[R]]
    // CHECK:   destroy_value [[BOX]]
    guard case .Branch(left: let l, right: let r) = tree else { return }

    // CHECK:   destroy_value [[MOVED_R]]
    // CHECK:   destroy_value [[MOVED_L]]
    // CHECK:   destroy_addr [[X]]
  }

  do {
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO4:bb[0-9]+]]
    // CHECK: [[NO4]]([[ORIGINAL_VALUE:%.*]] : @owned $TreeA<T>):
    // CHECK:   destroy_value [[ORIGINAL_VALUE]]
    // CHECK: [[YES]]:
    // CHECK:   br
    if case .Nil = tree { }

    // CHECK:   [[X:%.*]] = alloc_stack [lexical] [var_decl] $T
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Leaf!enumelt: [[YES:bb[0-9]+]], default [[NO5:bb[0-9]+]]
    // CHECK: [[NO5]]([[ORIGINAL_VALUE:%.*]] : @owned $TreeA<T>):
    // CHECK:   destroy_value [[ORIGINAL_VALUE]]
    // CHECK: [[YES]]([[BOX:%.*]] : @owned $<τ_0_0> { var τ_0_0 } <T>):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TMP:%.*]] = alloc_stack
    // CHECK:   copy_addr [[VALUE_ADDR]] to [init] [[TMP]]
    // CHECK:   copy_addr [take] [[TMP]] to [init] [[X]]
    // CHECK:   destroy_value [[BOX]]
    // CHECK:   destroy_addr [[X]]
    if case .Leaf(let x) = tree { }


    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   switch_enum [[ARG_COPY]] : $TreeA<T>, case #TreeA.Branch!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]([[ORIGINAL_VALUE:%.*]] : @owned $TreeA<T>):
    // CHECK:   destroy_value [[ORIGINAL_VALUE]]
    // CHECK: [[YES]]([[BOX:%.*]] : @owned $<τ_0_0> { var (left: TreeA<τ_0_0>, right: TreeA<τ_0_0>) } <T>):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TUPLE:%.*]] = load_borrow [[VALUE_ADDR]]
    // CHECK:   [[TUPLE_COPY:%.*]] = copy_value [[TUPLE]]
    // CHECK:   end_borrow [[TUPLE]]
    // CHECK:   ([[L:%.*]], [[R:%.*]]) = destructure_tuple [[TUPLE_COPY]]
    // CHECK:   [[MOVED_L:%.*]] = move_value [lexical] [var_decl] [[L]]
    // CHECK:   [[MOVED_R:%.*]] = move_value [lexical] [var_decl] [[R]]
    // CHECK:   destroy_value [[BOX]]
    // CHECK:   destroy_value [[MOVED_R]]
    // CHECK:   destroy_value [[MOVED_L]]
    if case .Branch(left: let l, right: let r) = tree { }
  }
  // CHECK: [[NO3]]([[ORIGINAL_VALUE:%.*]] : @owned $TreeA<T>):
  // CHECK:   destroy_value [[ORIGINAL_VALUE]]
  // CHECK: [[NO2]]([[ORIGINAL_VALUE:%.*]] : @owned $TreeA<T>):
  // CHECK:   destroy_value [[ORIGINAL_VALUE]]
  // CHECK: [[NO1]]([[ORIGINAL_VALUE:%.*]] : @owned $TreeA<T>):
  // CHECK:   destroy_value [[ORIGINAL_VALUE]]
}
// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum10guardTreeB{{[_0-9a-zA-Z]*}}F
func guardTreeB<T>(_ tree: TreeB<T>) {
  do {
    // CHECK:   copy_addr %0 to [init] [[TMP1:%.*]] :
    // CHECK:   switch_enum_addr [[TMP1]] : $*TreeB<T>, case #TreeB.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO2:bb[0-9]+]]
    // CHECK: [[YES]]:
    // CHECK:   destroy_addr [[TMP1]]
    guard case .Nil = tree else { return }

    // CHECK:   [[X:%.*]] = alloc_stack [lexical] [var_decl] $T
    // CHECK:   copy_addr %0 to [init] [[TMP2:%.*]] :
    // CHECK:   switch_enum_addr [[TMP2]] : $*TreeB<T>, case #TreeB.Leaf!enumelt: [[YES:bb[0-9]+]], default [[NO2:bb[0-9]+]]
    // CHECK: [[YES]]:
    // CHECK:   [[VALUE:%.*]] = unchecked_take_enum_data_addr [[TMP2]]
    // CHECK:   copy_addr [take] [[VALUE]] to [init] [[X]]
    // CHECK:   dealloc_stack [[TMP2]]
    guard case .Leaf(let x) = tree else { return }

    // CHECK:   [[L:%.*]] = alloc_stack [lexical] [var_decl] $TreeB
    // CHECK:   [[R:%.*]] = alloc_stack [lexical] [var_decl] $TreeB
    // CHECK:   copy_addr %0 to [init] [[TMP3:%.*]] :
    // CHECK:   switch_enum_addr [[TMP3]] : $*TreeB<T>, case #TreeB.Branch!enumelt: [[YES:bb[0-9]+]], default [[NO3:bb[0-9]+]]
    // CHECK: [[YES]]:
    // CHECK:   [[BOX_ADDR:%.*]] = unchecked_take_enum_data_addr [[TMP3]]
    // CHECK:   [[BOX:%.*]] = load [take] [[BOX_ADDR]]
    // CHECK:   [[TUPLE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   copy_addr [[TUPLE_ADDR]] to [init] [[TUPLE_COPY:%.*]] :
    // CHECK:   [[L_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   [[R_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[L_COPY]] to [init] [[L]]
    // CHECK:   copy_addr [take] [[R_COPY]] to [init] [[R]]
    // CHECK:   destroy_value [[BOX]]
    guard case .Branch(left: let l, right: let r) = tree else { return }

    // CHECK:   destroy_addr [[R]]
    // CHECK:   destroy_addr [[L]]
    // CHECK:   destroy_addr [[X]]
  }

  do {
    // CHECK:   copy_addr %0 to [init] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   destroy_addr [[TMP]]
    if case .Nil = tree { }

    // CHECK:   [[X:%.*]] = alloc_stack [lexical] [var_decl] $T
    // CHECK:   copy_addr %0 to [init] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Leaf!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   [[VALUE:%.*]] = unchecked_take_enum_data_addr [[TMP]]
    // CHECK:   copy_addr [take] [[VALUE]] to [init] [[X]]
    // CHECK:   dealloc_stack [[TMP]]
    // CHECK:   destroy_addr [[X]]
    if case .Leaf(let x) = tree { }

    // CHECK:   [[L:%.*]] = alloc_stack [lexical] [var_decl] $TreeB
    // CHECK:   [[R:%.*]] = alloc_stack [lexical] [var_decl] $TreeB
    // CHECK:   copy_addr %0 to [init] [[TMP:%.*]] :
    // CHECK:   switch_enum_addr [[TMP]] : $*TreeB<T>, case #TreeB.Branch!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   destroy_addr [[TMP]]
    // CHECK: [[YES]]:
    // CHECK:   [[BOX_ADDR:%.*]] = unchecked_take_enum_data_addr [[TMP]]
    // CHECK:   [[BOX:%.*]] = load [take] [[BOX_ADDR]]
    // CHECK:   [[TUPLE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   copy_addr [[TUPLE_ADDR]] to [init] [[TUPLE_COPY:%.*]] :
    // CHECK:   [[L_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   [[R_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[L_COPY]] to [init] [[L]]
    // CHECK:   copy_addr [take] [[R_COPY]] to [init] [[R]]
    // CHECK:   destroy_value [[BOX]]
    // CHECK:   destroy_addr [[R]]
    // CHECK:   destroy_addr [[L]]
    if case .Branch(left: let l, right: let r) = tree { }
  }
  // CHECK: [[NO3]]:
  // CHECK:   destroy_addr [[TMP3]]
  // CHECK: [[NO2]]:
  // CHECK:   destroy_addr [[TMP2]]
  // CHECK: [[NO1]]:
  // CHECK:   destroy_addr [[TMP1]]
}

// Just run guardTreeInt through the ownership verifier
//
// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum12guardTreeInt{{[_0-9a-zA-Z]*}}F
func guardTreeInt(_ tree: TreeInt) {
  do {
    guard case .Nil = tree else { return }

    guard case .Leaf(let x) = tree else { return }

    guard case .Branch(left: let l, right: let r) = tree else { return }
  }

  do {
    if case .Nil = tree { }
    if case .Leaf(let x) = tree { }
    if case .Branch(left: let l, right: let r) = tree { }
  }
}

// SEMANTIC ARC TODO: This test needs to be made far more comprehensive.
// CHECK-LABEL: sil hidden [ossa] @$s13indirect_enum35dontDisableCleanupOfIndirectPayloadyyAA010TrivialButG0OF : $@convention(thin) (@guaranteed TrivialButIndirect) -> () {
func dontDisableCleanupOfIndirectPayload(_ x: TrivialButIndirect) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $TrivialButIndirect):
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK:   switch_enum [[ARG_COPY]] : $TrivialButIndirect, case #TrivialButIndirect.Direct!enumelt:  [[YES:bb[0-9]+]], case #TrivialButIndirect.Indirect!enumelt: [[NO:bb[0-9]+]]
  //
  guard case .Direct(let foo) = x else { return }

  // CHECK: [[YES]]({{%.*}} : $Int):
  // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK:   switch_enum [[ARG_COPY]] : $TrivialButIndirect, case #TrivialButIndirect.Indirect!enumelt:  [[YES:bb[0-9]+]], case #TrivialButIndirect.Direct!enumelt: [[NO2:bb[0-9]+]]

  // CHECK: [[YES]]([[BOX:%.*]] : @owned ${ var Int }):
  // CHECK:   destroy_value [[BOX]]

  // CHECK: [[NO2]]({{%.*}} : $Int):
  // CHECK-NOT: destroy_value

  // CHECK: [[NO]]([[PAYLOAD:%.*]] : @owned ${ var Int }):
  // CHECK:   destroy_value [[PAYLOAD]]

  guard case .Indirect(let bar) = x else { return }
}
// CHECK: } // end sil function '$s13indirect_enum35dontDisableCleanupOfIndirectPayloadyyAA010TrivialButG0OF'

// Make sure that in these cases we do not break any ownership invariants.
class Box<T> {
  var value: T
  init(_ inputValue: T) { value = inputValue }
}

enum ValueWithInlineStorage<T> {
case inline(T)
indirect case box(Box<T>)
}

func switchValueWithInlineStorage<U>(v: ValueWithInlineStorage<U>) {
  switch v {
  case .inline:
    return
  case .box(let box):
    return
  }
}

func guardValueWithInlineStorage<U>(v: ValueWithInlineStorage<U>) {
  do {
    guard case .inline = v else { return }
    guard case .box(let box) = v else { return }
  }

  do {
    if case .inline = v { return }
    if case .box(let box) = v { return }
  }
}
