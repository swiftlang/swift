// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil hidden @_TF15optional_lvalue22assign_optional_lvalueFTRGSqSi_Si_T_
// CHECK:         [[SHADOW:%.*]] = alloc_box $Optional<Int>
// CHECK:         [[PRECOND:%.*]] = function_ref @_TFSs30_diagnoseUnexpectedNilOptionalFT_T_
// CHECK:         apply [[PRECOND]]()
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[SHADOW]]#1 : $*Optional<Int>, #Optional.Some!enumelt.1
// CHECK:         assign {{%.*}} to [[PAYLOAD]]
func assign_optional_lvalue(inout x: Int?, y: Int) {
  x! = y
}

// CHECK-LABEL: sil hidden @_TF15optional_lvalue17assign_iuo_lvalueFTRGSQSi_Si_T_
// CHECK:         [[SHADOW:%.*]] = alloc_box $ImplicitlyUnwrappedOptional<Int>
// CHECK:         [[PRECOND:%.*]] = function_ref @_TFSs30_diagnoseUnexpectedNilOptionalFT_T_
// CHECK:         apply [[PRECOND]]()
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[SHADOW]]#1 : $*ImplicitlyUnwrappedOptional<Int>, #ImplicitlyUnwrappedOptional.Some!enumelt.1
// CHECK:         assign {{%.*}} to [[PAYLOAD]]
func assign_iuo_lvalue(inout x: Int!, y: Int) {
  x! = y
}

struct S {
  var x: Int

  var computed: Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden @_TF15optional_lvalue26assign_iuo_lvalue_implicitFTRGSQVS_1S_Si_T_
// CHECK:         [[SHADOW:%.*]] = alloc_box
// CHECK:         [[SOME:%.*]] = unchecked_take_enum_data_addr [[SHADOW]]#1
// CHECK:         [[X:%.*]] = struct_element_addr [[SOME]]
func assign_iuo_lvalue_implicit(inout s: S!, y: Int) {
  s.x = y
}

// CHECK-LABEL: sil hidden @_TF15optional_lvalue35assign_optional_lvalue_reabstractedFTRGSqFSiSi_FSiSi_T_
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_TTRXFo_dSi_dSi_XFo_iSi_iSi_
// CHECK:         [[REABSTRACTED:%.*]] = partial_apply [[REABSTRACT]]
// CHECK:         assign [[REABSTRACTED]] to {{%.*}} : $*@callee_owned (@out Int, @in Int) -> ()
func assign_optional_lvalue_reabstracted(inout x: (Int -> Int)?,
                                         y: Int -> Int) {
  x! = y
}

// CHECK-LABEL: sil hidden @_TF15optional_lvalue31assign_optional_lvalue_computedFTRGSqVS_1S_Si_Si
// CHECK:         function_ref @_TFV15optional_lvalue1Ss8computedSi
// CHECK:         function_ref @_TFV15optional_lvalue1Sg8computedSi
func assign_optional_lvalue_computed(inout x: S?, y: Int) -> Int {
  x!.computed = y
  return x!.computed
}

func generate_int() -> Int { return 0 }

// CHECK-LABEL: sil hidden @_TF15optional_lvalue28assign_bound_optional_lvalueFRGSqSi_T_
// CHECK:         select_enum_addr
// CHECK:         cond_br {{%.*}}, [[SOME:bb[0-9]+]], [[NONE:bb[0-9]+]]
// CHECK:       [[SOME]]:
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr
// CHECK:         [[FN:%.*]] = function_ref
// CHECK:         [[T0:%.*]] = apply [[FN]]()
// CHECK:         assign [[T0]] to [[PAYLOAD]]
func assign_bound_optional_lvalue(inout x: Int?) {
  x? = generate_int()
}
