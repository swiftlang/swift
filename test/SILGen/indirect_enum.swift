// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

indirect enum TreeA<T> {
  case Nil
  case Leaf(T)
  case Branch(left: TreeA<T>, right: TreeA<T>)
}

// CHECK-LABEL: sil hidden @_TF13indirect_enum11TreeA_casesurFTx1lGOS_5TreeAx_1rGS0_x__T_
func TreeA_cases<T>(_ t: T, l: TreeA<T>, r: TreeA<T>) {

// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[NIL:%.*]] = enum $TreeA<T>, #TreeA.Nil!enumelt
// CHECK-NEXT:    release_value [[NIL]]
  let _ = TreeA<T>.Nil

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $T
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    copy_addr %0 to [initialization] [[PB]]
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeA<T>, #TreeA.Leaf!enumelt.1, [[BOX]]
// CHECK-NEXT:    release_value [[LEAF]]
  let _ = TreeA<T>.Leaf(t)

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeA<T>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $(left: TreeA<T>, right: TreeA<T>)
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]] : $*(left: TreeA<T>, right: TreeA<T>), 0
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]] : $*(left: TreeA<T>, right: TreeA<T>), 1
// CHECK-NEXT:    retain_value %1
// CHECK-NEXT:    store %1 to [[LEFT]]
// CHECK-NEXT:    retain_value %2
// CHECK-NEXT:    store %2 to [[RIGHT]]
// CHECK-NEXT:    [[BRANCH:%.*]] = enum $TreeA<T>, #TreeA.Branch!enumelt.1, [[BOX]]
// CHECK-NEXT:    release_value [[BRANCH]]
// CHECK-NEXT:    release_value %2
// CHECK-NEXT:    release_value %1
// CHECK-NEXT:    destroy_addr %0
  let _ = TreeA<T>.Branch(left: l, right: r)

// CHECK:         return

}

// CHECK-LABEL: sil hidden @_TF13indirect_enum16TreeA_reabstractFFSiSiT_
func TreeA_reabstract(_ f: @escaping (Int) -> Int) {

// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeA<(Int) -> Int>.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $@callee_owned (@in Int) -> @out Int
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    strong_retain %0
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFo_dSi_dSi_XFo_iSi_iSi_
// CHECK-NEXT:    [[FN:%.*]] = partial_apply [[THUNK]](%0)
// CHECK-NEXT:    store [[FN]] to [[PB]]
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeA<(Int) -> Int>, #TreeA.Leaf!enumelt.1, [[BOX]]
// CHECK-NEXT:    release_value [[LEAF]]
// CHECK-NEXT:    strong_release %0
// CHECK: return
  let _ = TreeA<(Int) -> Int>.Leaf(f)
}

enum TreeB<T> {
  case Nil
  case Leaf(T)
  indirect case Branch(left: TreeB<T>, right: TreeB<T>)
}

// CHECK-LABEL: sil hidden @_TF13indirect_enum11TreeB_casesurFTx1lGOS_5TreeBx_1rGS0_x__T_
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
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $(left: TreeB<T>, right: TreeB<T>)
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    copy_addr %1 to [initialization] [[LEFT]] : $*TreeB<T>
// CHECK-NEXT:    copy_addr %2 to [initialization] [[RIGHT]] : $*TreeB<T>
// CHECK-NEXT:    [[BRANCH:%.*]] = alloc_stack $TreeB<T>
// CHECK-NEXT:    [[PAYLOAD:%.*]] = init_enum_data_addr [[BRANCH]]
// CHECK-NEXT:    store [[BOX]] to [[PAYLOAD]]
// CHECK-NEXT:    inject_enum_addr [[BRANCH]] : $*TreeB<T>, #TreeB.Branch!enumelt.1
// CHECK-NEXT:    destroy_addr [[BRANCH]]
// CHECK-NEXT:    dealloc_stack [[BRANCH]]
// CHECK-NEXT:    destroy_addr %2
// CHECK-NEXT:    destroy_addr %1
// CHECK-NEXT:    destroy_addr %0
  let _ = TreeB<T>.Branch(left: l, right: r)

// CHECK:         return

}

// CHECK-LABEL: sil hidden @_TF13indirect_enum13TreeInt_casesFTSi1lOS_7TreeInt1rS0__T_ : $@convention(thin) (Int, @owned TreeInt, @owned TreeInt) -> ()
func TreeInt_cases(_ t: Int, l: TreeInt, r: TreeInt) {

// CHECK:         [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[NIL:%.*]] = enum $TreeInt, #TreeInt.Nil!enumelt
// CHECK-NEXT:    release_value [[NIL]]
  let _ = TreeInt.Nil

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[LEAF:%.*]] = enum $TreeInt, #TreeInt.Leaf!enumelt.1, %0
// CHECK-NEXT:    release_value [[LEAF]]
  let _ = TreeInt.Leaf(t)

// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin TreeInt.Type
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box $(left: TreeInt, right: TreeInt)
// CHECK-NEXT:    [[PB:%.*]] = project_box [[BOX]]
// CHECK-NEXT:    [[LEFT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    [[RIGHT:%.*]] = tuple_element_addr [[PB]]
// CHECK-NEXT:    retain_value %1
// CHECK-NEXT:    store %1 to [[LEFT]]
// CHECK-NEXT:    retain_value %2
// CHECK-NEXT:    store %2 to [[RIGHT]]
// CHECK-NEXT:    [[BRANCH:%.*]] = enum $TreeInt, #TreeInt.Branch!enumelt.1, [[BOX]]
// CHECK-NEXT:    release_value [[BRANCH]]
// CHECK-NEXT:    release_value %2
// CHECK-NEXT:    release_value %1
  let _ = TreeInt.Branch(left: l, right: r)

// CHECK:         return

}

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

// CHECK-LABEL: sil hidden @_TF13indirect_enum11switchTreeA
func switchTreeA<T>(_ x: TreeA<T>) {
  // --           x +2
  // CHECK:       retain_value %0
  // CHECK:       switch_enum %0 : $TreeA<T>
  switch x {
  // CHECK:     bb{{.*}}:
  // CHECK:       function_ref @_TF13indirect_enum1aFT_T_
  case .Nil:
    a()
  // CHECK:     bb{{.*}}([[LEAF_BOX:%.*]] : $@box T):
  // CHECK:       [[VALUE:%.*]] = project_box [[LEAF_BOX]]
  // CHECK:       copy_addr [[VALUE]] to [initialization] [[X:%.*]] : $*T
  // CHECK:       function_ref @_TF13indirect_enum1b
  // CHECK:       destroy_addr [[X]]
  // CHECK:       dealloc_stack [[X]]
  // --           x +1
  // CHECK:       strong_release [[LEAF_BOX]]
  // CHECK:       br [[OUTER_CONT:bb[0-9]+]]
  case .Leaf(let x):
    b(x)

  // CHECK:     bb{{.*}}([[NODE_BOX:%.*]] : $@box (left: TreeA<T>, right: TreeA<T>)):
  // CHECK:       [[TUPLE_ADDR:%.*]] = project_box [[NODE_BOX]]
  // CHECK:       [[TUPLE:%.*]] = load [[TUPLE_ADDR]]
  // CHECK:       [[LEFT:%.*]] = tuple_extract [[TUPLE]] {{.*}}, 0
  // CHECK:       [[RIGHT:%.*]] = tuple_extract [[TUPLE]] {{.*}}, 1
  // CHECK:       switch_enum [[RIGHT]] {{.*}}, default [[FAIL_RIGHT:bb[0-9]+]]

  // CHECK:     bb{{.*}}([[RIGHT_LEAF_BOX:%.*]] : $@box T):
  // CHECK:       [[RIGHT_LEAF_VALUE:%.*]] = project_box [[RIGHT_LEAF_BOX]]
  // CHECK:       switch_enum [[LEFT]] {{.*}}, default [[FAIL_LEFT:bb[0-9]+]]
  
  // CHECK:     bb{{.*}}([[LEFT_LEAF_BOX:%.*]] : $@box T):
  // CHECK:       [[LEFT_LEAF_VALUE:%.*]] = project_box [[LEFT_LEAF_BOX]]
  // CHECK:       copy_addr [[LEFT_LEAF_VALUE]]
  // CHECK:       copy_addr [[RIGHT_LEAF_VALUE]]
  // --           x +1
  // CHECK:       strong_release [[NODE_BOX]]
  // CHECK:       br [[OUTER_CONT]]

  // CHECK:     [[FAIL_LEFT]]:
  // CHECK:       br [[DEFAULT:bb[0-9]+]]

  // CHECK:     [[FAIL_RIGHT]]:
  // CHECK:       br [[DEFAULT]]

  case .Branch(.Leaf(let x), .Leaf(let y)):
    c(x, y)

  // CHECK:     [[DEFAULT]]:
  // --           x +1
  // CHECK:       release_value %0
  default:
    d()
  }

  // CHECK:     [[OUTER_CONT:%.*]]:
  // --           x +0
  // CHECK:       release_value %0 : $TreeA<T>
}

// CHECK-LABEL: sil hidden @_TF13indirect_enum11switchTreeB
func switchTreeB<T>(_ x: TreeB<T>) {
  // CHECK:       copy_addr %0 to [initialization] [[SCRATCH:%.*]] :
  // CHECK:       switch_enum_addr [[SCRATCH]]
  switch x {

  // CHECK:     bb{{.*}}:
  // CHECK:       destroy_addr [[SCRATCH]]
  // CHECK:       dealloc_stack [[SCRATCH]]
  // CHECK:       function_ref @_TF13indirect_enum1aFT_T_
  // CHECK:       br [[OUTER_CONT:bb[0-9]+]]
  case .Nil:
    a()

  // CHECK:     bb{{.*}}:
  // CHECK:       copy_addr [[SCRATCH]] to [initialization] [[LEAF_COPY:%.*]] :
  // CHECK:       [[LEAF_ADDR:%.*]] = unchecked_take_enum_data_addr [[LEAF_COPY]]
  // CHECK:       copy_addr [take] [[LEAF_ADDR]] to [initialization] [[LEAF:%.*]] :
  // CHECK:       function_ref @_TF13indirect_enum1b
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
  // CHECK:       copy_addr [[SCRATCH]] to [initialization] [[TREE_COPY:%.*]] :
  // CHECK:       [[TREE_ADDR:%.*]] = unchecked_take_enum_data_addr [[TREE_COPY]]
  // --           box +1 immutable
  // CHECK:       [[BOX:%.*]] = load [[TREE_ADDR]]
  // CHECK:       [[TUPLE:%.*]] = project_box [[BOX]]
  // CHECK:       [[LEFT:%.*]] = tuple_element_addr [[TUPLE]]
  // CHECK:       [[RIGHT:%.*]] = tuple_element_addr [[TUPLE]]
  // CHECK:       switch_enum_addr [[RIGHT]] {{.*}}, default [[RIGHT_FAIL:bb[0-9]+]]

  // CHECK:     bb{{.*}}:
  // CHECK:       copy_addr [[RIGHT]] to [initialization] [[RIGHT_COPY:%.*]] :
  // CHECK:       [[RIGHT_LEAF:%.*]] = unchecked_take_enum_data_addr [[RIGHT_COPY]] : $*TreeB<T>, #TreeB.Leaf
  // CHECK:       switch_enum_addr [[LEFT]] {{.*}}, default [[LEFT_FAIL:bb[0-9]+]]

  // CHECK:     bb{{.*}}:
  // CHECK:       copy_addr [[LEFT]] to [initialization] [[LEFT_COPY:%.*]] :
  // CHECK:       [[LEFT_LEAF:%.*]] = unchecked_take_enum_data_addr [[LEFT_COPY]] : $*TreeB<T>, #TreeB.Leaf
  // CHECK:       copy_addr [take] [[LEFT_LEAF]] to [initialization] [[X:%.*]] :
  // CHECK:       copy_addr [take] [[RIGHT_LEAF]] to [initialization] [[Y:%.*]] :
  // CHECK:       function_ref @_TF13indirect_enum1c
  // CHECK:       destroy_addr [[Y]]
  // CHECK:       dealloc_stack [[Y]]
  // CHECK:       destroy_addr [[X]]
  // CHECK:       dealloc_stack [[X]]
  // CHECK-NOT:   destroy_addr [[LEFT_COPY]]
  // CHECK:       dealloc_stack [[LEFT_COPY]]
  // CHECK-NOT:   destroy_addr [[RIGHT_COPY]]
  // CHECK:       dealloc_stack [[RIGHT_COPY]]
  // --           box +0
  // CHECK:       strong_release [[BOX]]
  // CHECK-NOT:   destroy_addr [[TREE_COPY]]
  // CHECK:       dealloc_stack [[TREE_COPY]]
  // CHECK:       destroy_addr [[SCRATCH]]
  // CHECK:       dealloc_stack [[SCRATCH]]
  case .Branch(.Leaf(let x), .Leaf(let y)):
    c(x, y)

  // CHECK:     [[LEFT_FAIL]]:
  // CHECK:       destroy_addr [[RIGHT_LEAF]]
  // CHECK-NOT:   destroy_addr [[RIGHT_COPY]]
  // CHECK:       dealloc_stack [[RIGHT_COPY]]
  // CHECK:       strong_release [[BOX]]
  // CHECK-NOT:   destroy_addr [[TREE_COPY]]
  // CHECK:       dealloc_stack [[TREE_COPY]]
  // CHECK:       br [[INNER_CONT:bb[0-9]+]]

  // CHECK:     [[RIGHT_FAIL]]:
  // CHECK:       strong_release [[BOX]]
  // CHECK-NOT:   destroy_addr [[TREE_COPY]]
  // CHECK:       dealloc_stack [[TREE_COPY]]
  // CHECK:       br [[INNER_CONT:bb[0-9]+]]

  // CHECK:     [[INNER_CONT]]:
  // CHECK:       destroy_addr [[SCRATCH]]
  // CHECK:       dealloc_stack [[SCRATCH]]
  // CHECK:       function_ref @_TF13indirect_enum1dFT_T_
  // CHECK:       br [[OUTER_CONT]]
  default:
    d()
  }
  // CHECK:     [[OUTER_CONT]]:
  // CHECK:       destroy_addr %0
}

// CHECK-LABEL: sil hidden @_TF13indirect_enum10guardTreeA
func guardTreeA<T>(_ tree: TreeA<T>) {
  do {
    // CHECK:   retain_value %0
    // CHECK:   switch_enum %0 : $TreeA<T>, case #TreeA.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   release_value %0
    // CHECK: [[YES]]:
    // CHECK:   release_value %0
    guard case .Nil = tree else { return }

    // CHECK:   [[X:%.*]] = alloc_stack $T
    // CHECK:   retain_value %0
    // CHECK:   switch_enum %0 : $TreeA<T>, case #TreeA.Leaf!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   release_value %0
    // CHECK: [[YES]]([[BOX:%.*]] : $@box T):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TMP:%.*]] = alloc_stack
    // CHECK:   copy_addr [[VALUE_ADDR]] to [initialization] [[TMP]]
    // CHECK:   copy_addr [take] [[TMP]] to [initialization] [[X]]
    // CHECK:   strong_release [[BOX]]
    guard case .Leaf(let x) = tree else { return }

    // CHECK:   retain_value %0
    // CHECK:   switch_enum %0 : $TreeA<T>, case #TreeA.Branch!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   release_value %0
    // CHECK: [[YES]]([[BOX:%.*]] : $@box (left: TreeA<T>, right: TreeA<T>)):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TUPLE:%.*]] = load [[VALUE_ADDR]]
    // CHECK:   retain_value [[TUPLE]]
    // CHECK:   [[L:%.*]] = tuple_extract [[TUPLE]]
    // CHECK:   [[R:%.*]] = tuple_extract [[TUPLE]]
    // CHECK:   strong_release [[BOX]]
    guard case .Branch(left: let l, right: let r) = tree else { return }

    // CHECK:   release_value [[R]]
    // CHECK:   release_value [[L]]
    // CHECK:   destroy_addr [[X]]
  }

  do {
    // CHECK:   retain_value %0
    // CHECK:   switch_enum %0 : $TreeA<T>, case #TreeA.Nil!enumelt: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   release_value %0
    // CHECK: [[YES]]:
    // CHECK:   release_value %0
    if case .Nil = tree { }

    // CHECK:   [[X:%.*]] = alloc_stack $T
    // CHECK:   retain_value %0
    // CHECK:   switch_enum %0 : $TreeA<T>, case #TreeA.Leaf!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   release_value %0
    // CHECK: [[YES]]([[BOX:%.*]] : $@box T):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TMP:%.*]] = alloc_stack
    // CHECK:   copy_addr [[VALUE_ADDR]] to [initialization] [[TMP]]
    // CHECK:   copy_addr [take] [[TMP]] to [initialization] [[X]]
    // CHECK:   strong_release [[BOX]]
    // CHECK:   destroy_addr [[X]]
    if case .Leaf(let x) = tree { }


    // CHECK:   retain_value %0
    // CHECK:   switch_enum %0 : $TreeA<T>, case #TreeA.Branch!enumelt.1: [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
    // CHECK: [[NO]]:
    // CHECK:   release_value %0
    // CHECK: [[YES]]([[BOX:%.*]] : $@box (left: TreeA<T>, right: TreeA<T>)):
    // CHECK:   [[VALUE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   [[TUPLE:%.*]] = load [[VALUE_ADDR]]
    // CHECK:   retain_value [[TUPLE]]
    // CHECK:   [[L:%.*]] = tuple_extract [[TUPLE]]
    // CHECK:   [[R:%.*]] = tuple_extract [[TUPLE]]
    // CHECK:   strong_release [[BOX]]
    // CHECK:   release_value [[R]]
    // CHECK:   release_value [[L]]
    if case .Branch(left: let l, right: let r) = tree { }
  }
}

// CHECK-LABEL: sil hidden @_TF13indirect_enum10guardTreeB
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
    // CHECK:   [[BOX:%.*]] = load [[BOX_ADDR]]
    // CHECK:   [[TUPLE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   copy_addr [[TUPLE_ADDR]] to [initialization] [[TUPLE_COPY:%.*]] :
    // CHECK:   [[L_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[L_COPY]] to [initialization] [[L]]
    // CHECK:   [[R_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[R_COPY]] to [initialization] [[R]]
    // CHECK:   strong_release [[BOX]]
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
    // CHECK:   [[BOX:%.*]] = load [[BOX_ADDR]]
    // CHECK:   [[TUPLE_ADDR:%.*]] = project_box [[BOX]]
    // CHECK:   copy_addr [[TUPLE_ADDR]] to [initialization] [[TUPLE_COPY:%.*]] :
    // CHECK:   [[L_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[L_COPY]] to [initialization] [[L]]
    // CHECK:   [[R_COPY:%.*]] = tuple_element_addr [[TUPLE_COPY]]
    // CHECK:   copy_addr [take] [[R_COPY]] to [initialization] [[R]]
    // CHECK:   strong_release [[BOX]]
    // CHECK:   destroy_addr [[R]]
    // CHECK:   destroy_addr [[L]]
    if case .Branch(left: let l, right: let r) = tree { }
  }
}

func dontDisableCleanupOfIndirectPayload(_ x: TrivialButIndirect) {
  // CHECK:   switch_enum %0 : $TrivialButIndirect, case #TrivialButIndirect.Direct!enumelt.1:  [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
  // CHECK: [[NO]]:
  // CHECK:   release_value %0
  guard case .Direct(let foo) = x else { return }

  // -- Cleanup isn't necessary on "no" path because .Direct is trivial
  // CHECK:   switch_enum %0 : $TrivialButIndirect, case #TrivialButIndirect.Indirect!enumelt.1:  [[YES:bb[0-9]+]], default [[NO:bb[0-9]+]]
  // CHECK-NOT: [[NO]]:
  // CHECK: [[YES]]({{.*}}):
  guard case .Indirect(let bar) = x else { return }
}
