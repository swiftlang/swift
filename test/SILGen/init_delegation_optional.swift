// 'try?' on delegations to 'Optional' initializers should never flatten
// optionals, or else we do not discern the difference between a failure and a
// constructed value. Run in compatibility modes that disable and enable
// the flattening to verify this.
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s -swift-version 5 | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s -swift-version 4.2 | %FileCheck %s

extension Optional {
  init(nonFailable1: ()) {
    self = .none
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalE12nonFailable2xSgyt_tcfC
  init(nonFailable2: ()) {
    // CHECK: bb0([[OUT:%[0-9]+]] : $*Optional<Wrapped>, [[SELF_META:%[0-9]+]] : $@thin Optional<Wrapped>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <Wrapped>, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Wrapped>
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE12nonFailable1xSgyt_tcfC
    // CHECK-NEXT: apply [[DELEG_INIT]]<Wrapped>([[RESULT_ADDR]], [[SELF_META]])
    self.init(nonFailable1: ())
    // CHECK-NEXT: copy_addr [take] [[RESULT_ADDR]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
    // CHECK-NEXT: copy_addr [[PB]] to [init] [[OUT]]
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
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Wrapped>
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE12nonFailable1xSgyt_tcfC
    // CHECK-NEXT: apply [[DELEG_INIT]]<Wrapped>([[RESULT_ADDR]], [[SELF_META]])
    self.init(nonFailable1: ())
    // CHECK-NEXT: copy_addr [take] [[RESULT_ADDR]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
    // CHECK-NEXT: [[OUT_SOME_ADDR:%[0-9]+]] = init_enum_data_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [[PB]] to [init] [[OUT_SOME_ADDR]]
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
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
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
    // CHECK-NEXT: copy_addr [[PB]] to [init] [[OUT_SOME_ADDR]]
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

  init(throws: ()) throws {
    self = .none
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalE9failable3xSgSgyt_tcfC
  init?(failable3: ()) {
    // CHECK: bb0([[OUT:%[0-9]+]] : $*Optional<Optional<Wrapped>>, [[SELF_META:%[0-9]+]] : $@thin Optional<Wrapped>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <Wrapped>, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[OPT_RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Wrapped>>
    // CHECK-NEXT: [[OPT_RESULT_DATA_ADDR:%[0-9]+]] = init_enum_data_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE6throwsxSgyt_tKcfC
    // CHECK-NEXT: try_apply [[DELEG_INIT]]<Wrapped>([[OPT_RESULT_DATA_ADDR]], [[SELF_META]]) : {{.*}}, normal [[SUCC_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    try? self.init(throws: ())
    //
    // CHECK: [[SUCC_BB]]
    // CHECK-NEXT: inject_enum_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: br bb2
    //
    // CHECK: bb2:
    // CHECK: [[SELECT:%[0-9]+]] = select_enum_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: cond_br [[SELECT]], [[SOME_BB:bb[0-9]]], [[NONE_BB:bb[0-9]]]
    //
    // CHECK: [[NONE_BB]]:
    // CHECK-NEXT: destroy_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: br bb5
    //
    // CHECK: [[SOME_BB]]:
    // CHECK-NEXT: [[RESULT_ADDR:%[0-9]+]] = unchecked_take_enum_data_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [take] [[RESULT_ADDR]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: [[OUT_DATA_ADDR:%[0-9]+]] = init_enum_data_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [[PB]] to [init] [[OUT_DATA_ADDR]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb6
    //
    // CHECK: bb5:
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb6
    //
    // CHECK: bb6:
    // CHECK-NEXT: [[RET:%[0-9]+]] = tuple ()
    // CHECK-NEXT: return [[RET]] : $()
    //
    // CHECK: bb7([[ERR:%[0-9]+]] : @owned $any Error):
    // CHECK-NEXT: destroy_value [[ERR]]
    // CHECK-NEXT: inject_enum_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb2
    //
    // CHECK: [[ERROR_BB]]([[ERR:%[0-9]+]] : @owned $any Error):
    // CHECK-NEXT: br bb7([[ERR]] : $any Error)
    // CHECK-NEXT: }
  }

  init?(failableAndThrows: ()) throws {
    self = .none
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalE9failable4xSgSgyt_tcfC
  init?(failable4: ()) {
    // CHECK: bb0([[OUT:%[0-9]+]] : $*Optional<Optional<Wrapped>>, [[SELF_META:%[0-9]+]] : $@thin Optional<Wrapped>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <Wrapped>, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[OPT_OPT_RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Optional<Wrapped>>>
    // CHECK-NEXT: [[OPT_OPT_RESULT_DATA_ADDR:%[0-9]+]] = init_enum_data_addr [[OPT_OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE17failableAndThrowsxSgSgyt_tKcfC
    // CHECK-NEXT: try_apply [[DELEG_INIT]]<Wrapped>([[OPT_OPT_RESULT_DATA_ADDR]], [[SELF_META]]) : {{.*}}, normal [[SUCC_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    try? self.init(failableAndThrows: ())
    //
    // CHECK: [[SUCC_BB]]
    // CHECK-NEXT: inject_enum_addr [[OPT_OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: br bb2
    //
    // CHECK: bb2:
    // CHECK-NEXT: [[OPT_RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Wrapped>>
    // CHECK-NEXT: switch_enum_addr [[OPT_OPT_RESULT_ADDR]] : {{.*}}, case #Optional.some!enumelt: [[OPT_OPT_SOME_BB:bb[0-9]]], case #Optional.none!enumelt: [[OPT_OPT_NONE_BB:bb[0-9]]]
    //
    // CHECK: [[OPT_OPT_SOME_BB]]:
    // CHECK-NEXT: [[DATA_ADDR:%[0-9]+]] = unchecked_take_enum_data_addr [[OPT_OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [[DATA_ADDR]] to [init] [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: destroy_addr [[DATA_ADDR]]
    // CHECK-NEXT: br bb5
    //
    // CHECK: [[OPT_OPT_NONE_BB]]:
    // CHECK-NEXT: inject_enum_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb5
    //
    // CHECK: bb5:
    // CHECK: [[SELECT:%[0-9]+]] = select_enum_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: cond_br [[SELECT]], [[OPT_SOME_BB:bb[0-9]]], [[OPT_NONE_BB:bb[0-9]]]
    //
    // CHECK: [[OPT_NONE_BB]]:
    // CHECK-NEXT: destroy_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: dealloc_stack [[OPT_OPT_RESULT_ADDR]]
    // CHECK-NEXT: br bb8
    //
    // CHECK: [[OPT_SOME_BB]]:
    // CHECK-NEXT: [[RESULT_ADDR:%[0-9]+]] = unchecked_take_enum_data_addr [[OPT_RESULT_ADDR]] : $*Optional<Optional<Wrapped>>, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [take] [[RESULT_ADDR]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: dealloc_stack [[OPT_OPT_RESULT_ADDR]]
    // CHECK-NEXT: [[OUT_DATA_ADDR:%[0-9]+]] = init_enum_data_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [[PB]] to [init] [[OUT_DATA_ADDR]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb9
    //
    // CHECK: bb8:
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb9
    //
    // CHECK: bb9:
    // CHECK-NEXT: [[RET:%[0-9]+]] = tuple ()
    // CHECK-NEXT: return [[RET]] : $()
    //
    // CHECK: bb10([[ERROR:%[0-9]+]] : @owned $any Error):
    // CHECK-NEXT: destroy_value [[ERROR]]
    // CHECK-NEXT: inject_enum_addr [[OPT_OPT_RESULT_ADDR]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb2
    //
    // CHECK: [[ERROR_BB]]([[ERROR:%[0-9]+]] : @owned $any Error):
    // CHECK-NEXT: br bb10([[ERROR]] : $any Error)
    // CHECK-NEXT: }
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalE9failable5xSgSgyt_tcfC
  init?(failable5: ()) {
    // CHECK: bb0([[OUT:%[0-9]+]] : $*Optional<Optional<Wrapped>>, [[SELF_META:%[0-9]+]] : $@thin Optional<Wrapped>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <Wrapped>, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[OPT_RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Wrapped>>
    // CHECK-NEXT: [[OPT_RESULT_DATA_ADDR:%[0-9]+]] = init_enum_data_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: [[TMP_OPT_RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Wrapped>>
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE17failableAndThrowsxSgSgyt_tKcfC
    // CHECK-NEXT: try_apply [[DELEG_INIT]]<Wrapped>([[TMP_OPT_RESULT_ADDR]], [[SELF_META]]) : {{.*}}, normal [[SUCC_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    try? self.init(failableAndThrows: ())!
    //
    // CHECK: [[SUCC_BB]]
    // CHECK-NEXT: switch_enum_addr [[TMP_OPT_RESULT_ADDR]] : {{.*}}, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
    //
    // CHECK: [[NONE_BB]]:
    // CHECK: unreachable
    //
    // CHECK: [[SOME_BB]]:
    // CHECK-NEXT: [[TMP_RESULT_ADDR:%[0-9]+]] = unchecked_take_enum_data_addr [[TMP_OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [take] [[TMP_RESULT_ADDR]] to [init] [[OPT_RESULT_DATA_ADDR]]
    // CHECK-NEXT: inject_enum_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: dealloc_stack [[TMP_OPT_RESULT_ADDR]]
    // CHECK-NEXT: br bb4
    //
    // CHECK: bb4:
    // CHECK: [[SELECT:%[0-9]+]] = select_enum_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: cond_br [[SELECT]], [[SOME_BB:bb[0-9]]], [[NONE_BB:bb[0-9]]]
    //
    // CHECK: [[NONE_BB]]:
    // CHECK-NEXT: destroy_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: br bb7
    //
    // CHECK: [[SOME_BB]]:
    // CHECK-NEXT: [[RESULT_ADDR:%[0-9]+]] = unchecked_take_enum_data_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [take] [[RESULT_ADDR]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: [[OUT_DATA_ADDR:%[0-9]+]] = init_enum_data_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [[PB]] to [init] [[OUT_DATA_ADDR]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb8
    //
    // CHECK: bb7:
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb8
    //
    // CHECK: bb8:
    // CHECK-NEXT: [[RET:%[0-9]+]] = tuple ()
    // CHECK-NEXT: return [[RET]] : $()
    //
    // CHECK: bb9([[ERROR:%[0-9]+]] : @owned $any Error):
    // CHECK-NEXT: destroy_value [[ERROR]]
    // CHECK-NEXT: inject_enum_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb4
    //
    // CHECK: [[ERROR_BB]]([[ERROR:%[0-9]+]] : @owned $any Error):
    // CHECK-NEXT: dealloc_stack [[TMP_OPT_RESULT_ADDR]]
    // CHECK-NEXT: br bb9([[ERROR]] : $any Error)
    // CHECK-NEXT: }
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalE9failable6xSgSgyt_tcfC
  init?(failable6: ()) {
    // CHECK: bb0([[OUT:%[0-9]+]] : $*Optional<Optional<Wrapped>>, [[SELF_META:%[0-9]+]] : $@thin Optional<Wrapped>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <Wrapped>, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[SELF_LIFETIME]]
    // CHECK: [[OPT_RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Wrapped>>
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE17failableAndThrowsxSgSgyt_tKcfC
    // CHECK-NEXT: try_apply [[DELEG_INIT]]<Wrapped>([[OPT_RESULT_ADDR]], [[SELF_META]]) : {{.*}}, normal [[SUCC_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    try! self.init(failableAndThrows: ())
    //
    // CHECK: [[SUCC_BB]]
    // CHECK: [[SELECT:%[0-9]+]] = select_enum_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: cond_br [[SELECT]], [[SOME_BB:bb[0-9]]], [[NONE_BB:bb[0-9]]]
    //
    // CHECK: [[NONE_BB]]:
    // CHECK-NEXT: destroy_addr [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: br bb4
    //
    // CHECK: [[SOME_BB]]:
    // CHECK-NEXT: [[RESULT_ADDR:%[0-9]+]] = unchecked_take_enum_data_addr [[OPT_RESULT_ADDR]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [take] [[RESULT_ADDR]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[OPT_RESULT_ADDR]]
    // CHECK-NEXT: [[OUT_DATA_ADDR:%[0-9]+]] = init_enum_data_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: copy_addr [[PB]] to [init] [[OUT_DATA_ADDR]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb5
    //
    // CHECK: bb4:
    // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: inject_enum_addr [[OUT]] : {{.*}}, #Optional.none!enumelt
    // CHECK-NEXT: br bb5
    //
    // CHECK: bb5:
    // CHECK-NEXT: [[RET:%[0-9]+]] = tuple ()
    // CHECK-NEXT: return [[RET]] : $()
    //
    // CHECK: bb6({{%[0-9]+}} : @owned $any Error):
    // CHECK: unreachable
    //
    // CHECK: [[ERROR_BB]]([[ERROR:%[0-9]+]] : @owned $any Error):
    // CHECK: br bb6([[ERROR]] : $any Error)
    // CHECK-NEXT: }
  }
}

extension Optional where Wrapped == Optional<Bool> {
  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalSbSgRszlE13SpecFailable1ABSgSgyt_tcfC
  init?(SpecFailable1: ()) {
    // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin Optional<Optional<Bool>>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Optional<Optional<Bool>> }, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[MARKED_SELF_LIFETIME:%[0-9]+]] = begin_borrow [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[MARKED_SELF_LIFETIME]]
    // CHECK: [[RESULT_ADDR:%[0-9]+]] = alloc_stack $Optional<Optional<Bool>>
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalE12nonFailable1xSgyt_tcfC
    // CHECK-NEXT: apply [[DELEG_INIT]]<Bool?>([[RESULT_ADDR]], [[SELF_META]])
    // CHECK-NEXT: [[RESULT:%[0-9]+]] = load [trivial] [[RESULT_ADDR]]
    // CHECK-NEXT: assign [[RESULT]] to [[PB]]
    // CHECK-NEXT: dealloc_stack [[RESULT_ADDR]]
    // CHECK-NEXT: [[RESULT:%[0-9]+]] = load [trivial] [[PB]]
    // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<Optional<Optional<Bool>>>, #Optional.some!enumelt, [[RESULT]]
    // CHECK-NEXT: end_borrow [[MARKED_SELF_LIFETIME]]
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
    // CHECK-NEXT: [[MARKED_SELF_LIFETIME:%[0-9]+]] = begin_borrow [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[MARKED_SELF_LIFETIME]]
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
    // CHECK-NEXT: end_borrow [[MARKED_SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb4([[INJECT_INTO_OPT]] : $Optional<Optional<Optional<Bool>>>)
    //
    // CHECK: bb3:
    // CHECK-NEXT: end_borrow [[MARKED_SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[NIL:%[0-9]+]] = enum $Optional<Optional<Optional<Bool>>>, #Optional.none!enumelt
    // CHECK-NEXT: br bb4([[NIL]] : $Optional<Optional<Optional<Bool>>>)
    //
    // CHECK: bb4([[RET:%[0-9]+]] : $Optional<Optional<Optional<Bool>>>):
    // CHECK-NEXT: return [[RET]]
    // CHECK-NEXT: }
    self.init(SpecFailable1: ())
  }

  init?(SpecFailableAndThrows: ()) throws {
    self = .none
  }

  // CHECK-LABEL: sil hidden [ossa] @$sSq24init_delegation_optionalSbSgRszlE13SpecFailable3ABSgSgyt_tcfC
  init?(SpecFailable3: ()) {
    // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin Optional<Optional<Bool>>.Type):
    // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Optional<Optional<Bool>> }, var
    // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK-NEXT: [[MARKED_SELF_LIFETIME:%[0-9]+]] = begin_borrow [var_decl] [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[PB:%[0-9]+]] = project_box [[MARKED_SELF_LIFETIME]]
    // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$sSq24init_delegation_optionalSbSgRszlE21SpecFailableAndThrowsABSgSgyt_tKcfC
    // CHECK-NEXT: try_apply [[DELEG_INIT]]([[SELF_META]]) : {{.*}}, normal [[SUCC_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    //
    // CHECK: [[SUCC_BB]]([[OPT_RESULT:%[0-9]+]] : $Optional<Optional<Optional<Bool>>>):
    // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<Optional<Optional<Optional<Bool>>>>, #Optional.some!enumelt, [[OPT_RESULT]]
    // CHECK-NEXT: br bb2([[INJECT_INTO_OPT]] : $Optional<Optional<Optional<Optional<Bool>>>>)
    //
    // CHECK: bb2([[OPT_OPT_RESULT:%[0-9]+]] : $Optional<Optional<Optional<Optional<Bool>>>>):
    // CHECK-NEXT: switch_enum [[OPT_OPT_RESULT]] : {{.*}}, case #Optional.some!enumelt: [[OPT_OPT_SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[OPT_OPT_NONE_BB:bb[0-9]+]]
    //
    // CHECK: [[OPT_OPT_SOME_BB]]([[OPT_RESULT:%[0-9]+]] : $Optional<Optional<Optional<Bool>>>):
    // CHECK-NEXT: br bb5([[OPT_RESULT]] : $Optional<Optional<Optional<Bool>>>)
    //
    // CHECK: [[OPT_OPT_NONE_BB]]:
    // CHECK-NEXT: [[NIL:%[0-9]+]] = enum $Optional<Optional<Optional<Bool>>>, #Optional.none!enumelt
    // CHECK-NEXT: br bb5([[NIL]] : $Optional<Optional<Optional<Bool>>>)
    //
    // CHECK: bb5([[OPT_RESULT:%[0-9]+]] : $Optional<Optional<Optional<Bool>>>):
    // CHECK: [[SELECT:%[0-9]+]] = select_enum [[OPT_RESULT]]
    // CHECK-NEXT: cond_br [[SELECT]], [[OPT_SOME_BB:bb[0-9]+]], [[OPT_NONE_BB:bb[0-9]+]]
    //
    // CHECK: [[OPT_NONE_BB]]:
    // CHECK-NEXT: br bb8
    //
    // CHECK: [[OPT_SOME_BB]]:
    // CHECK-NEXT: [[RESULT:%[0-9]+]] = unchecked_enum_data [[OPT_RESULT]] : {{.*}}, #Optional.some!enumelt
    // CHECK-NEXT: assign [[RESULT]] to [[PB]]
    // CHECK-NEXT: [[RESULT:%[0-9]+]] = load [trivial] [[PB]]
    // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<Optional<Optional<Bool>>>, #Optional.some!enumelt, [[RESULT]]
    // CHECK-NEXT: end_borrow [[MARKED_SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: br bb9([[INJECT_INTO_OPT]] : $Optional<Optional<Optional<Bool>>>)
    //
    // CHECK: bb8:
    // CHECK-NEXT: end_borrow [[MARKED_SELF_LIFETIME]]
    // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
    // CHECK-NEXT: [[NIL:%[0-9]+]] = enum $Optional<Optional<Optional<Bool>>>, #Optional.none!enumelt
    // CHECK-NEXT: br bb9([[NIL]] : $Optional<Optional<Optional<Bool>>>)
    //
    // CHECK: bb9([[RET:%[0-9]+]] : $Optional<Optional<Optional<Bool>>>):
    // CHECK-NEXT: return [[RET]]
    //
    // CHECK: bb10([[ERROR:%[0-9]+]] : @owned $any Error):
    // CHECK-NEXT: destroy_value [[ERROR]]
    // CHECK-NEXT: [[NIL:%[0-9]+]] = enum $Optional<Optional<Optional<Optional<Bool>>>>, #Optional.none!enumelt
    // CHECK-NEXT: br bb2([[NIL]] : $Optional<Optional<Optional<Optional<Bool>>>>)
    //
    // CHECK: [[ERROR_BB]]([[ERROR:%[0-9]+]] : @owned $any Error):
    // CHECK-NEXT: br bb10([[ERROR]] : $any Error)
    // CHECK-NEXT: }
    try? self.init(SpecFailableAndThrows: ())
  }
}
