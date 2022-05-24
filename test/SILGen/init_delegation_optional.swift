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

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalE9failable1xSgSgyt_tcfC
  init?(failable1: ()) {
    // CHECK: bb0([[OUT:%[0-9]+]] : $*Optional<Optional<Wrapped>>, [[SELF_META:%[0-9]+]] : $@thin Optional<Wrapped>.Type):
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
    // CHECK-NEXT: [[OUT_SOME_ADDR:%[0-9]+]] = init_enum_data_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [[PB]] to [initialization] [[OUT_SOME_ADDR]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb2
    //
    // FIXME: Dead branch
    // CHECK: bb1:
    //
    // CHECK: bb2:
    // CHECK-NEXT: [[RET:%[0-9]+]] = tuple ()
    // CHECK-NEXT: return [[RET]] : $()
    // CHECK-NEXT: }
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalE9failable2xSgSgyt_tcfC
  init?(failable2: ()) {
    // CHECK: bb0([[OUT:%[0-9]+]] : $*Optional<Optional<Wrapped>>, [[SELF_META:%[0-9]+]] : $@thin Optional<Wrapped>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <Wrapped>, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[OPT_RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Wrapped>>
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE9failable1xSgSgyt_tcfC
    // CHECK-NEXT: apply [[DELEG_INIT]]<Wrapped>([[OPT_RESULT_ADDR]], [[SELF_META]])
    self.init(failable1: ())
    // CHECK: [[SELECT:%[0-9]+]] = select_enum_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: cond_br [[SELECT]], [[SOME_BB:bb[0-9]]], [[NONE_BB:bb[0-9]]]
    //
    // CHECK: [[NONE_BB]]:
    // CHECK-NEXT: destroy_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: br bb3
    //
    // CHECK: [[SOME_BB]]:
    // CHECK-NEXT: [[RESULT_ADDR:%[0-9]+]] = unchecked_take_enum_data_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [take] [[RESULT_ADDR]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: [[OUT_SOME_ADDR:%[0-9]+]] = init_enum_data_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [[PB]] to [initialization] [[OUT_SOME_ADDR]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb4
    //
    // CHECK: bb3:
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb4
    //
    // CHECK: bb4:
    // CHECK-NEXT: [[RET:%[0-9]+]] = tuple ()
    // CHECK-NEXT: return [[RET]] : $()
    // CHECK-NEXT: }
  }
}

extension Optional where Wrapped == Optional<Bool> {
  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalSbSgRszlE13SpecFailable1ABSgSgyt_tcfC
  init?(SpecFailable1: ()) {
    // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin Optional<Optional<Bool>>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Optional<Optional<Bool>> }, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Bool>>
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE12nonFailable1xSgyt_tcfC
    // CHECK-NEXT: apply [[DELEG_INIT]]<Bool?>([[RESULT_ADDR]], [[SELF_META]])
    // CHECK-NEXT: [[RESULT:%[0-9]+]] = load [trivial] [[RESULT_ADDR]]
    // CHECK-NEXT: assign [[RESULT]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
    // CHECK-NEXT: [[RESULT:%[0-9]+]] = load [trivial] [[PB]]
    // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<Optional<Optional<Bool>>>, #Optional.some!enumelt, [[RESULT]]
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb2([[INJECT_INTO_OPT]] : $Optional<Optional<Optional<Bool>>>)
    //
    // FIXME: Dead branch
    // CHECK: bb1:
    //
    // CHECK: bb2([[RET:%[0-9]+]] : $Optional<Optional<Optional<Bool>>>):
    // CHECK-NEXT: return [[RET]]
    // CHECK-NEXT: }
    self.init(nonFailable1: ())
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalSbSgRszlE13SpecFailable2ABSgSgyt_tcfC
  init?(SpecFailable2: ()) {
    // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin Optional<Optional<Bool>>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Optional<Optional<Bool>> }, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalSbSgRszlE13SpecFailable1ABSgSgyt_tcfC
    // CHECK-NEXT: [[OPT_RESULT:%[0-9]+]] = apply [[DELEG_INIT]]([[SELF_META]])
    // CHECK: [[SELECT:%[0-9]+]] = select_enum [[OPT_RESULT]]
    // CHECK-NEXT: cond_br [[SELECT]], [[SOME_BB:bb[0-9]]], [[NONE_BB:bb[0-9]]]
    //
    // CHECK: [[NONE_BB]]:
    // CHECK-NEXT: br bb3
    //
    // CHECK: [[SOME_BB]]:
    // CHECK-NEXT: [[RESULT:%[0-9]+]] = unchecked_enum_data [[OPT_RESULT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: assign [[RESULT]] to [[PB]]
    // CHECK-NEXT: [[RESULT:%[0-9]+]] = load [trivial] [[PB]]
    // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<Optional<Optional<Bool>>>, #Optional.some!enumelt, [[RESULT]]
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb4([[INJECT_INTO_OPT]] : $Optional<Optional<Optional<Bool>>>)
    //
    // CHECK: bb3:
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[NIL:%[0-9]+]] = enum $Optional<Optional<Optional<Bool>>>, #Optional.none!enumelt
    // CHECK-NEXT: br bb4([[NIL]] : $Optional<Optional<Optional<Bool>>>)
    //
    // CHECK: bb4([[RET:%[0-9]+]] : $Optional<Optional<Optional<Bool>>>):
    // CHECK-NEXT: return [[RET]]
    // CHECK-NEXT: }
    self.init(SpecFailable1: ())
  }
}
