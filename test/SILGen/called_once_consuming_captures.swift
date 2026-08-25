// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

struct Resource: ~Copyable {
  init() {}
  consuming func use() {}
  borrowing func peek() {}
}

struct Box: ~Copyable {
  private var _r: Resource
  init(_ r: consuming Resource) { _r = r }

  var r: Resource {
    consuming get { _r }
  }

  var borrowedR: Resource {
    get { fatalError() }
  }
}

struct Slot: ~Copyable {
  private var _r: Resource = Resource()
  
  var r: Resource {
    get { fatalError() }
    consuming set { _r = newValue }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures35testExplicitConsumeOfCapturedStructyyAA8ResourceVnF : $@convention(thin) (@owned Resource) -> () {
// CHECK: bb0([[R:%.*]] : @owned $Resource):
// CHECK:  [[R_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[R_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_PROJ]] : $*Resource
// CHECK:  [[R_VALUE:%.*]] = load [take] [[R_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply {{.*}}([[R_VALUE]]) : $@convention(thin) (@owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures35testExplicitConsumeOfCapturedStructyyAA8ResourceVnF'
func testExplicitConsumeOfCapturedStruct(_ r: consuming Resource) {
  let g = { @called(once) in
    let taken = consume r
    taken.use()
  }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures36testAssignmentConsumesCapturedStructyyAA8ResourceVnF : $@convention(thin) (@owned Resource) -> () {
// CHECK: bb0([[R:%.*]] : @owned $Resource):
// CHECK:  [[R_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[R_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_PROJ]] : $*Resource
// CHECK:  [[R_VALUE:%.*]] = load [take] [[R_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply {{.*}}([[R_VALUE]]) : $@convention(thin) (@owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures36testAssignmentConsumesCapturedStructyyAA8ResourceVnF'
func testAssignmentConsumesCapturedStruct(_ r: consuming Resource) {
  let g = { @called(once) in
    var local = Resource()
    local = r
    local.use()
  }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures37testConsumingParamCallConsumesCaptureyyAA8ResourceVnF : $@convention(thin) (@owned Resource) -> () {
// CHECK: bb0([[R:%.*]] : @owned $Resource):
// CHECK:  [[R_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[R_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_PROJ]] : $*Resource
// CHECK:  [[R_VALUE:%.*]] = load [take] [[R_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply {{.*}}([[R_VALUE]]) : $@convention(thin) (@owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures37testConsumingParamCallConsumesCaptureyyAA8ResourceVnF'
func testConsumingParamCallConsumesCapture(_ r: consuming Resource) {
  func consumeResource(_ r: consuming Resource) {}

  let g = { @called(once) in
    consumeResource(r)
  }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures37testConsumingInitParamConsumesCaptureyyAA8ResourceVnF : $@convention(thin) (@owned Resource) -> () {
// CHECK: bb0([[R:%.*]] : @owned $Resource):
// CHECK:  [[R_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[R_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_PROJ]] : $*Resource
// CHECK:  [[R_VALUE:%.*]] = load [take] [[R_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply {{.*}}([[R_VALUE]]) : $@convention(thin) (@owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures37testConsumingInitParamConsumesCaptureyyAA8ResourceVnF'
func testConsumingInitParamConsumesCapture(_ r: consuming Resource) {
  struct Wrapper: ~Copyable {
    let r: Resource
  }

  let g = { @called(once) in
    _ = Wrapper(r: r)
  }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures38testConsumingMethodCallConsumesCaptureyyAA8ResourceVnF : $@convention(thin) (@owned Resource) -> () {
// CHECK: bb0([[R:%.*]] : @owned $Resource):
// CHECK:  [[R_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[R_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_PROJ]] : $*Resource
// CHECK:  [[R_VALUE:%.*]] = load [take] [[R_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply {{.*}}([[R_VALUE]]) : $@convention(thin) (@owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures38testConsumingMethodCallConsumesCaptureyyAA8ResourceVnF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures38testConsumingMethodCallConsumesCaptureyyAA8ResourceVnFyyXOfU_ : $@convention(thin) (@owned Resource) -> () {
// CHECK: bb0([[R_CAPTURE:%.*]] : @closureCapture @owned $Resource):
// CHECK:  [[R_STACK:%.*]] = alloc_stack $Resource, var, name "r"
// CHECK:  [[R_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_STACK]] : $*Resource
// CHECK:  store [[R_CAPTURE]] to [init] [[R_ADDR]] : $*Resource
// CHECK:  [[R_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_ADDR]] : $*Resource
// CHECK:  [[R_DEINIT_ACCESS:%.*]] = begin_access [deinit] [unknown] [[R_ADDR_2]] : $*Resource
// CHECK:  [[R_TAKEN:%.*]] = load [take] [[R_DEINIT_ACCESS]] : $*Resource
// CHECK:  [[USE_REF:%.*]] = function_ref @$s30called_once_consuming_captures8ResourceV3useyyF
// CHECK:  apply [[USE_REF]]([[R_TAKEN]]) : $@convention(method) (@owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures38testConsumingMethodCallConsumesCaptureyyAA8ResourceVnFyyXOfU_'
func testConsumingMethodCallConsumesCapture(_ r: consuming Resource) {
  let g = { @called(once) in
    r.use()
  }
  g()
}

// A plain local `let taken = r` binding of a captured value is a consuming
// use, just like an explicit `consume`.
// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures31testLocalBindingConsumesCaptureyyAA8ResourceVnF : $@convention(thin) (@owned Resource) -> () {
// CHECK: bb0([[R:%.*]] : @owned $Resource):
// CHECK:  [[R_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[R_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_PROJ]] : $*Resource
// CHECK:  [[R_VALUE:%.*]] = load [take] [[R_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply {{.*}}([[R_VALUE]]) : $@convention(thin) (@owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures31testLocalBindingConsumesCaptureyyAA8ResourceVnF'
func testLocalBindingConsumesCapture(_ r: consuming Resource) {
  let g = { @called(once) in
    let taken = r
    taken.use()
  }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures36testBorrowingUsesDoNotConsumeCaptureyyAA8ResourceVF : $@convention(thin) (@guaranteed Resource) -> () {
// CHECK: bb0([[R:%.*]] : @guaranteed $Resource):
// CHECK:  [[R_COPY:%.*]] = copy_value [[R]] : $Resource
// CHECK:  [[R_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[R_COPY]] : $Resource
// CHECK:  [[R_ARG_COPY:%.*]] = copy_value [[R_ADDR]] : $Resource
// CHECK:  partial_apply {{.*}}([[R_ARG_COPY]]) : $@convention(thin) (@guaranteed Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures36testBorrowingUsesDoNotConsumeCaptureyyAA8ResourceVF'
func testBorrowingUsesDoNotConsumeCapture(_ r: borrowing Resource) {
  func borrowResource(_ r: borrowing Resource) {}

  let g = { @called(once) in
    r.peek()
    borrowResource(r)
  }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures38testMixedConsumingAndBorrowingCapturesyyAA8ResourceVn_ADtF : $@convention(thin) (@owned Resource, @guaranteed Resource) -> () {
// CHECK: bb0([[CONSUMED:%.*]] : @owned $Resource, [[BORROWED:%.*]] : @guaranteed $Resource)
// CHECK:  [[CONSUMED_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[BORROWED_COPY:%.*]] = copy_value [[BORROWED]] : $Resource
// CHECK:  [[BORROWED_ADDR:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[BORROWED_COPY]] : $Resource
// CHECK:  [[BORROWED_ARG_COPY:%.*]] = copy_value [[BORROWED_ADDR]] : $Resource
// CHECK:  [[CONSUMED_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[CONSUMED_PROJ]] : $*Resource
// CHECK:  [[CONSUMED_VALUE:%.*]] = load [take] [[CONSUMED_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply {{.*}}([[BORROWED_ARG_COPY]], [[CONSUMED_VALUE]]) : $@convention(thin) (@guaranteed Resource, @owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures38testMixedConsumingAndBorrowingCapturesyyAA8ResourceVn_ADtF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures38testMixedConsumingAndBorrowingCapturesyyAA8ResourceVn_ADtFyyXOfU_ : $@convention(thin) (@guaranteed Resource, @owned Resource) -> () {
// CHECK: bb0([[BORROWED_CAPTURE:%.*]] : @closureCapture @guaranteed $Resource, [[CONSUMED_CAPTURE:%.*]] : @closureCapture @owned $Resource):
func testMixedConsumingAndBorrowingCaptures(_ consumed: consuming Resource, _ borrowed: borrowing Resource) {
  let g = { @called(once) in
    borrowed.peek()
    consumed.use()
  }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntF : $@convention(thin) (@owned Resource, @owned Resource) -> () {
// CHECK: bb0([[R1:%.*]] : @owned $Resource, [[R2:%.*]] : @owned $Resource):
// CHECK:  [[R1_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[R2_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[CLOSURE_G:%.*]] = function_ref @$s30called_once_consuming_captures27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOfU_
// CHECK:  [[R1_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R1_PROJ]] : $*Resource
// CHECK:  [[R1_VALUE:%.*]] = load [take] [[R1_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply [called_once] [[CLOSURE_G]]({{.*}}, [[R1_VALUE]]) : $@convention(thin) (@guaranteed { var S }, @owned Resource) -> ()
// CHECK:  [[CLOSURE_H:%.*]] = function_ref @$s30called_once_consuming_captures27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOfU0_
// CHECK:  [[R2_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R2_PROJ]] : $*Resource
// CHECK:  [[R2_VALUE:%.*]] = load [take] [[R2_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply [called_once] [[CLOSURE_H]]({{.*}}, [[R2_VALUE]]) : $@convention(thin) (@guaranteed C, @owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOfU_ : $@convention(thin) (@guaranteed { var S }, @owned Resource) -> () {
// CHECK: bb0([[S_CAPTURE:%.*]] : @closureCapture @guaranteed ${ var S }, [[R1_CAPTURE:%.*]] : @closureCapture @owned $Resource):
// CHECK:  [[S_PROJ:%.*]] = project_box [[S_CAPTURE]] : ${ var S }, 0
// CHECK:  [[R1_STACK:%.*]] = alloc_stack $Resource, var, name "r1"
// CHECK:  [[R1_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R1_STACK]] : $*Resource
// CHECK:  store [[R1_CAPTURE]] to [init] [[R1_ADDR]] : $*Resource
// CHECK:  [[R1_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R1_ADDR]] : $*Resource
// CHECK:  [[R1_READ_ACCESS:%.*]] = begin_access [read] [unknown] [[R1_ADDR_2]] : $*Resource
// CHECK:  [[R1_FAKE_COPY:%.*]] = load [copy] [[R1_READ_ACCESS]] : $*Resource
// CHECK:  end_access [[R1_READ_ACCESS]] : $*Resource
// CHECK:  [[S_ACCESS:%.*]] = begin_access [modify] [unknown] [[S_PROJ]] : $*S
// CHECK:  [[S_WRITE_ADDR:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[S_ACCESS]] : $*S
// CHECK:  [[PROP_ADDR:%.*]] = struct_element_addr [[S_WRITE_ADDR]] : $*S, #{{.*}}S.prop
// CHECK:  assign [[R1_FAKE_COPY]] to [[PROP_ADDR]] : $*Resource
// CHECK:  end_access [[S_ACCESS]] : $*S
// CHECK: } // end sil function '$s30called_once_consuming_captures27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOfU_'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOfU0_ : $@convention(thin) (@guaranteed C, @owned Resource) -> () {
// CHECK: bb0([[C_CAPTURE:%.*]] : @closureCapture @guaranteed $C, [[R2_CAPTURE:%.*]] : @closureCapture @owned $Resource):
// CHECK:  [[R2_STACK:%.*]] = alloc_stack $Resource, var, name "r2"
// CHECK:  [[R2_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R2_STACK]] : $*Resource
// CHECK:  store [[R2_CAPTURE]] to [init] [[R2_ADDR]] : $*Resource
// CHECK:  [[R2_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R2_ADDR]] : $*Resource
// CHECK:  [[R2_READ_ACCESS:%.*]] = begin_access [read] [unknown] [[R2_ADDR_2]] : $*Resource
// CHECK:  [[R2_FAKE_COPY:%.*]] = load [copy] [[R2_READ_ACCESS]] : $*Resource
// CHECK:  end_access [[R2_READ_ACCESS]] : $*Resource
// CHECK:  [[SETTER:%.*]] = class_method [[C_CAPTURE]] : $C, #{{.*}}C.prop!setter
// CHECK:  apply [[SETTER]]([[R2_FAKE_COPY]], [[C_CAPTURE]]) : $@convention(method) (@owned Resource, @guaranteed C) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures27tesReassignmentOfProperties2r12r2yAA8ResourceVn_AFntFyyXOfU0_'
func tesReassignmentOfProperties(r1: consuming Resource, r2: consuming Resource) {
  struct S: ~Copyable {
    var prop = Resource()
  }

  class C {
    var prop = Resource()
  }

  var s = S()
  let c = C()

  let g = { @called(once) in
    s.prop = r1
  }
  g()

  let h = { @called(once) in
    c.prop = r2
  }
  h()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures34testConsumingGetterConsumesCaptureyyAA3BoxVnF : $@convention(thin) (@owned Box) -> () {
// CHECK: bb0([[BOX:%.*]] : @owned $Box):
// CHECK:  [[BOX_PROJ:%.*]] = project_box {{.*}} : ${ var Box }, 0
// CHECK:  [[BOX_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[BOX_PROJ]] : $*Box
// CHECK:  [[BOX_VALUE:%.*]] = load [take] [[BOX_TAKE_ADDR]] : $*Box
// CHECK:  partial_apply {{.*}}([[BOX_VALUE]]) : $@convention(thin) (@owned Box) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures34testConsumingGetterConsumesCaptureyyAA3BoxVnF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures34testConsumingGetterConsumesCaptureyyAA3BoxVnFyyXOfU_ : $@convention(thin) (@owned Box) -> () {
// CHECK: bb0([[BOX_CAPTURE:%.*]] : @closureCapture @owned $Box):
// CHECK:  [[BOX_STACK:%.*]] = alloc_stack $Box, var, name "box"
// CHECK:  [[BOX_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[BOX_STACK]] : $*Box
// CHECK:  store [[BOX_CAPTURE]] to [init] [[BOX_ADDR]] : $*Box
// CHECK:  [[BOX_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[BOX_ADDR]] : $*Box
// CHECK:  [[BOX_READ_ACCESS:%.*]] = begin_access [read] [unknown] [[BOX_ADDR_2]] : $*Box
// CHECK:  [[BOX_BORROW:%.*]] = load_borrow [unchecked] [[BOX_READ_ACCESS]] : $*Box
// CHECK:  [[BOX_COPY:%.*]] = copy_value [[BOX_BORROW]] : $Box
// CHECK:  [[GETTER:%.*]] = function_ref @$s30called_once_consuming_captures3BoxV1rAA8ResourceVvg : $@convention(method) (@owned Box) -> @owned Resource
// CHECK:  [[R_VALUE:%.*]] = apply [[GETTER]]([[BOX_COPY]]) : $@convention(method) (@owned Box) -> @owned Resource
// CHECK:  end_borrow [[BOX_BORROW]] : $Box
// CHECK:  end_access [[BOX_READ_ACCESS]] : $*Box
// CHECK: } // end sil function '$s30called_once_consuming_captures34testConsumingGetterConsumesCaptureyyAA3BoxVnFyyXOfU_'
func testConsumingGetterConsumesCapture(_ box: consuming Box) {
  let g = { @called(once) in
    let v = box.r
    _ = v
  }
  g()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures34testConsumingSetterConsumesCaptureyyAA4SlotVn_AA8ResourceVntF : $@convention(thin) (@owned Slot, @owned Resource) -> () {
// CHECK: bb0([[SLOT:%.*]] : @owned $Slot, [[R:%.*]] : @owned $Resource):
// CHECK:  [[SLOT_PROJ:%.*]] = project_box {{.*}} : ${ var Slot }, 0
// CHECK:  [[R_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[SLOT_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[SLOT_PROJ]] : $*Slot
// CHECK:  [[SLOT_VALUE:%.*]] = load [take] [[SLOT_TAKE_ADDR]] : $*Slot
// CHECK:  [[R_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_PROJ]] : $*Resource
// CHECK:  [[R_VALUE:%.*]] = load [take] [[R_TAKE_ADDR]] : $*Resource
// CHECK:  partial_apply {{.*}}([[SLOT_VALUE]], [[R_VALUE]]) : $@convention(thin) (@owned Slot, @owned Resource) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures34testConsumingSetterConsumesCaptureyyAA4SlotVn_AA8ResourceVntF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures34testConsumingSetterConsumesCaptureyyAA4SlotVn_AA8ResourceVntFyyXOfU_ : $@convention(thin) (@owned Slot, @owned Resource) -> () {
// CHECK: bb0([[SLOT_CAPTURE:%.*]] : @closureCapture @owned $Slot, [[R_CAPTURE:%.*]] : @closureCapture @owned $Resource):
// CHECK:  [[SLOT_STACK:%.*]] = alloc_stack $Slot, var, name "slot"
// CHECK:  [[SLOT_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[SLOT_STACK]] : $*Slot
// CHECK:  store [[SLOT_CAPTURE]] to [init] [[SLOT_ADDR]] : $*Slot
// CHECK:  [[SLOT_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[SLOT_ADDR]] : $*Slot
// CHECK:  [[R_STACK:%.*]] = alloc_stack $Resource, var, name "r"
// CHECK:  [[R_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_STACK]] : $*Resource
// CHECK:  store [[R_CAPTURE]] to [init] [[R_ADDR]] : $*Resource
// CHECK:  [[R_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[R_ADDR]] : $*Resource
// CHECK:  [[R_READ_ACCESS:%.*]] = begin_access [read] [unknown] [[R_ADDR_2]] : $*Resource
// CHECK:  [[R_FAKE_COPY:%.*]] = load [copy] [[R_READ_ACCESS]] : $*Resource
// CHECK:  end_access [[R_READ_ACCESS]] : $*Resource
// CHECK:  [[SLOT_READ_ACCESS:%.*]] = begin_access [read] [unknown] [[SLOT_ADDR_2]] : $*Slot
// CHECK:  [[SLOT_BORROW:%.*]] = load_borrow [unchecked] [[SLOT_READ_ACCESS]] : $*Slot
// CHECK:  [[SLOT_COPY:%.*]] = copy_value [[SLOT_BORROW]] : $Slot
// CHECK:  [[SETTER:%.*]] = function_ref @$s30called_once_consuming_captures4SlotV1rAA8ResourceVvs : $@convention(method) (@owned Resource, @owned Slot) -> ()
// CHECK:  apply [[SETTER]]([[R_FAKE_COPY]], [[SLOT_COPY]]) : $@convention(method) (@owned Resource, @owned Slot) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures34testConsumingSetterConsumesCaptureyyAA4SlotVn_AA8ResourceVntFyyXOfU_'
func testConsumingSetterConsumesCapture(_ slot: consuming Slot, _ r: consuming Resource) {
  let g = { @called(once) in
    slot.r = r
  }
  g()
}

protocol Usable: ~Copyable {
  consuming func use()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures40testGenericConsumingCaptureIsAddressOnlyyyxnAA6UsableRzRi_zlF : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> () {
// CHECK: bb0(%0 : $*T):
// CHECK:  [[T_PROJ:%.*]] = project_box {{.*}} : $<{{.*}}> { var {{.*}} } <T>, 0
// CHECK:  [[T_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[T_PROJ]] : $*T
// CHECK:  [[T_STACK:%.*]] = alloc_stack $T
// CHECK:  copy_addr [take] [[T_TAKE_ADDR]] to [init] [[T_STACK]] : $*T
// CHECK:  partial_apply [called_once] {{.*}}<T>([[T_STACK]]) : $@convention(thin) <{{.*}}> (@in {{.*}}) -> ()
// CHECK: } // end sil function '$s30called_once_consuming_captures40testGenericConsumingCaptureIsAddressOnlyyyxnAA6UsableRzRi_zlF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures40testGenericConsumingCaptureIsAddressOnlyyyxnAA6UsableRzRi_zlFyyXOfU_ : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> () {
// CHECK: bb0([[T_CAPTURE:%.*]] : @closureCapture $*T):
// CHECK:  [[T_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[T_CAPTURE]] : $*T
// CHECK:  [[T_DEINIT_ACCESS:%.*]] = begin_access [deinit] [unknown] [[T_ADDR]] : $*T
// CHECK:  witness_method $T, #Usable.use
// CHECK:  end_access [[T_DEINIT_ACCESS]] : $*T
// CHECK: } // end sil function '$s30called_once_consuming_captures40testGenericConsumingCaptureIsAddressOnlyyyxnAA6UsableRzRi_zlFyyXOfU_'
func testGenericConsumingCaptureIsAddressOnly<T: Usable & ~Copyable>(_ t: consuming T) {
  let g = { @called(once) in
    t.use()
  }
  g()
}

struct UsableResource: ~Copyable, Usable {
  consuming func use() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures16testIdentityCastyAA8ResourceVADnF : $@convention(thin) (@owned Resource) -> @owned Resource {
// CHECK: bb0([[X:%.*]] : @owned $Resource):
// CHECK:  [[X_PROJ:%.*]] = project_box {{.*}} : ${ var Resource }, 0
// CHECK:  [[X_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[X_PROJ]] : $*Resource
// CHECK:  [[X_VALUE:%.*]] = load [take] [[X_TAKE_ADDR]] : $*Resource
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s30called_once_consuming_captures16testIdentityCastyAA8ResourceVADnFADyXEfU_
// CHECK:  apply [[CLOSURE]]([[X_VALUE]]) : $@convention(thin) (@owned Resource) -> @owned Resource
// CHECK: } // end sil function '$s30called_once_consuming_captures16testIdentityCastyAA8ResourceVADnF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures16testIdentityCastyAA8ResourceVADnFADyXEfU_ : $@convention(thin) (@owned Resource) -> @owned Resource {
// CHECK: bb0([[X_CAPTURE:%.*]] : @closureCapture @owned $Resource):
// CHECK:  [[X_STACK:%.*]] = alloc_stack $Resource, var, name "x"
// CHECK:  [[X_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[X_STACK]] : $*Resource
// CHECK:  store [[X_CAPTURE]] to [init] [[X_ADDR]] : $*Resource
// CHECK:  [[X_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[X_ADDR]] : $*Resource
// CHECK:  [[X_READ_ACCESS:%.*]] = begin_access [read] [unknown] [[X_ADDR_2]] : $*Resource
// CHECK:  [[X_FAKE_COPY:%.*]] = load [copy] [[X_READ_ACCESS]] : $*Resource
// CHECK:  end_access [[X_READ_ACCESS]] : $*Resource
// CHECK:  destroy_addr [[X_ADDR_2]] : $*Resource
// CHECK:  return [[X_FAKE_COPY]] : $Resource
// CHECK: } // end sil function '$s30called_once_consuming_captures16testIdentityCastyAA8ResourceVADnFADyXEfU_'
func testIdentityCast(_ x: consuming Resource) -> Resource {
  { @called(once) in x as Resource }()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures24testEraseConsumesCaptureyyAA14UsableResourceVnF : $@convention(thin) (@owned UsableResource) -> () {
// CHECK: bb0([[C:%.*]] : @owned $UsableResource):
// CHECK:  [[C_PROJ:%.*]] = project_box {{.*}} : ${ var UsableResource }, 0
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s30called_once_consuming_captures24testEraseConsumesCaptureyyAA14UsableResourceVnFAA0I0_pRi_s_XPyXOfU_
// CHECK:  [[C_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[C_PROJ]] : $*UsableResource
// CHECK:  [[C_VALUE:%.*]] = load [take] [[C_TAKE_ADDR]] : $*UsableResource
// CHECK:  partial_apply [called_once] [[CLOSURE]]([[C_VALUE]]) : $@convention(thin) (@owned UsableResource) -> @out any Usable & ~Copyable
// CHECK: } // end sil function '$s30called_once_consuming_captures24testEraseConsumesCaptureyyAA14UsableResourceVnF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures24testEraseConsumesCaptureyyAA14UsableResourceVnFAA0I0_pRi_s_XPyXOfU_ : $@convention(thin) (@owned UsableResource) -> @out any Usable & ~Copyable {
// CHECK: bb0([[RET:%.*]] : $*any Usable & ~Copyable, [[C_CAPTURE:%.*]] : @closureCapture @owned $UsableResource):
// CHECK:  [[C_STACK:%.*]] = alloc_stack $UsableResource, var, name "c"
// CHECK:  [[C_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[C_STACK]] : $*UsableResource
// CHECK:  store [[C_CAPTURE]] to [init] [[C_ADDR]] : $*UsableResource
// CHECK:  [[C_ADDR_2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[C_ADDR]] : $*UsableResource
// CHECK:  [[C_READ_ACCESS:%.*]] = begin_access [read] [unknown] [[C_ADDR_2]] : $*UsableResource
// CHECK:  [[C_FAKE_COPY:%.*]] = load [copy] [[C_READ_ACCESS]] : $*UsableResource
// CHECK:  end_access [[C_READ_ACCESS]] : $*UsableResource
// CHECK:  [[EXISTENTIAL_ADDR:%.*]] = init_existential_addr [[RET]] : $*any Usable & ~Copyable, $UsableResource
// CHECK:  store [[C_FAKE_COPY]] to [init] [[EXISTENTIAL_ADDR]] : $*UsableResource
// CHECK:  destroy_addr [[C_ADDR_2]] : $*UsableResource
// CHECK: } // end sil function '$s30called_once_consuming_captures24testEraseConsumesCaptureyyAA14UsableResourceVnFAA0I0_pRi_s_XPyXOfU_'
func testEraseConsumesCapture(_ c: consuming UsableResource) {
  let fn = { @called(once) in c as any Usable & ~Copyable }
  _ = fn()
}

// CHECK-LABEL: sil hidden [ossa] @$s30called_once_consuming_captures31testGenericEraseConsumesCaptureyyxnAA6UsableRzRi_zlF : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> () {
// CHECK: bb0(%0 : $*T):
// CHECK:  [[V_PROJ:%.*]] = project_box {{.*}} : $<{{.*}}> { var {{.*}} } <T>, 0
// CHECK:  copy_addr [take] %0 to [init] [[V_PROJ]] : $*T
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s30called_once_consuming_captures31testGenericEraseConsumesCaptureyyxnAA6UsableRzRi_zlFAaC_pRi_s_XPyXOfU_
// CHECK:  [[V_TAKE_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[V_PROJ]] : $*T
// CHECK:  [[V_STACK:%.*]] = alloc_stack $T
// CHECK:  copy_addr [take] [[V_TAKE_ADDR]] to [init] [[V_STACK]] : $*T
// CHECK:  partial_apply [called_once] [[CLOSURE]]<T>([[V_STACK]]) : $@convention(thin) <{{.*}}> (@in {{.*}}) -> @out any Usable & ~Copyable
// CHECK: } // end sil function '$s30called_once_consuming_captures31testGenericEraseConsumesCaptureyyxnAA6UsableRzRi_zlF'

// CHECK-LABEL: sil private [ossa] @$s30called_once_consuming_captures31testGenericEraseConsumesCaptureyyxnAA6UsableRzRi_zlFAaC_pRi_s_XPyXOfU_ : $@convention(thin) <T where T : Usable, T : ~Copyable> (@in T) -> @out any Usable & ~Copyable {
// CHECK: bb0([[RET:%.*]] : $*any Usable & ~Copyable, [[V_CAPTURE:%.*]] : @closureCapture $*T):
// CHECK:  [[V_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[V_CAPTURE]] : $*T
// CHECK:  [[V_READ_ACCESS:%.*]] = begin_access [read] [unknown] [[V_ADDR]] : $*T
// CHECK:  [[EXISTENTIAL_ADDR:%.*]] = init_existential_addr [[RET]] : $*any Usable & ~Copyable, $T
// CHECK:  copy_addr [[V_READ_ACCESS]] to [init] [[EXISTENTIAL_ADDR]] : $*T
// CHECK:  end_access [[V_READ_ACCESS]] : $*T
// CHECK:  destroy_addr [[V_ADDR]] : $*T
// CHECK: } // end sil function '$s30called_once_consuming_captures31testGenericEraseConsumesCaptureyyxnAA6UsableRzRi_zlFAaC_pRi_s_XPyXOfU_'
func testGenericEraseConsumesCapture<T: Usable & ~Copyable>(_ v: consuming T) {
  let fn = { @called(once) in v as any Usable & ~Copyable }
  _ = fn()
}
