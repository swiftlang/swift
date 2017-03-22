// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden @_T015optional_lvalue07assign_a1_B0ySiSgz_SitF
// CHECK:         [[PRECOND:%.*]] = function_ref @_T0s30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]](
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr %0 : $*Optional<Int>, #Optional.some!enumelt.1
// CHECK:         assign {{%.*}} to [[PAYLOAD]]
func assign_optional_lvalue(_ x: inout Int?, _ y: Int) {
  x! = y
}

// CHECK-LABEL: sil hidden @_T015optional_lvalue011assign_iuo_B0ySQySiGz_SitF
// CHECK:         [[PRECOND:%.*]] = function_ref @_T0s30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]](
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr %0 : $*Optional<Int>, #Optional.some!enumelt.1
// CHECK:         assign {{%.*}} to [[PAYLOAD]]
func assign_iuo_lvalue(_ x: inout Int!, _ y: Int) {
  x! = y
}

struct S {
  var x: Int

  var computed: Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden @_T015optional_lvalue011assign_iuo_B9_implicitySQyAA1SVGz_SitF
// CHECK:         [[SOME:%.*]] = unchecked_take_enum_data_addr %0
// CHECK:         [[X:%.*]] = struct_element_addr [[SOME]]
func assign_iuo_lvalue_implicit(_ s: inout S!, _ y: Int) {
  s.x = y
}

struct Struct<T> {
  var value: T?
}

// CHECK-LABEL: sil hidden @_T015optional_lvalue07assign_a1_B13_reabstractedyAA6StructVyS2icGz_S2ictF
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_T0S2iIxyd_S2iIxir_TR
// CHECK:         [[REABSTRACTED:%.*]] = partial_apply [[REABSTRACT]]
// CHECK:         assign [[REABSTRACTED]] to {{%.*}} : $*@callee_owned (@in Int) -> @out Int
func assign_optional_lvalue_reabstracted(_ x: inout Struct<(Int) -> Int>,
                                         _ y: @escaping (Int) -> Int) {
  x.value! = y
}

// CHECK-LABEL: sil hidden @_T015optional_lvalue07assign_a1_B9_computedSiAA1SVSgz_SitF
// CHECK:         function_ref @_T015optional_lvalue1SV8computedSifs
// CHECK:         function_ref @_T015optional_lvalue1SV8computedSifg
func assign_optional_lvalue_computed(_ x: inout S?, _ y: Int) -> Int {
  x!.computed = y
  return x!.computed
}

func generate_int() -> Int { return 0 }

// CHECK-LABEL: sil hidden @_T015optional_lvalue013assign_bound_a1_B0ySiSgzF
// CHECK:         select_enum_addr
// CHECK:         cond_br {{%.*}}, [[SOME:bb[0-9]+]], [[NONE:bb[0-9]+]]
// CHECK:       [[SOME]]:
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr
// CHECK:         [[FN:%.*]] = function_ref
// CHECK:         [[T0:%.*]] = apply [[FN]]()
// CHECK:         assign [[T0]] to [[PAYLOAD]]
func assign_bound_optional_lvalue(_ x: inout Int?) {
  x? = generate_int()
}
