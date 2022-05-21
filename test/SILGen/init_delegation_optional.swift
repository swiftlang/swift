// RUN: %target-swift-emit-silgen %s | %FileCheck %s

extension Optional {
  init(nonFailable1: ()) {
    self = .none
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalE12nonFailable2xSgyt_tcfC
  init(nonFailable2: ()) {
    // CHECK: bb0([[OUT:%[0-9]+]] : $*Optional<Wrapped>, [[SELF_META:%[0-9]+]] : $@thin Optional<Wrapped>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <Wrapped>, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Wrapped>
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE12nonFailable1xSgyt_tcfC
    // CHECK-NEXT: apply [[DELEG_INIT]]<Wrapped>([[RESULT_ADDR]], [[SELF_META]])
    self.init(nonFailable1: ())
    // CHECK-NEXT: copy_addr [take] [[RESULT_ADDR]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
    // CHECK-NEXT: copy_addr [[PB]] to [initialization] [[OUT]]
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[RET:%[0-9]+]] = tuple ()
    // CHECK-NEXT: return [[RET]] : $()
    // CHECK-NEXT: }
  }
}
