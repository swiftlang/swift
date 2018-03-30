
// RUN: %target-swift-frontend -module-name optional_lvalue -emit-silgen -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$S15optional_lvalue07assign_a1_B0yySiSgz_SitF
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Optional<Int>
// CHECK:         [[PRECOND:%.*]] = function_ref @$Ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]](
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[WRITE]] : $*Optional<Int>, #Optional.some!enumelt.1
// CHECK:         assign {{%.*}} to [[PAYLOAD]]
func assign_optional_lvalue(_ x: inout Int?, _ y: Int) {
  x! = y
}

// CHECK-LABEL: sil hidden @$S15optional_lvalue011assign_iuo_B0yySiSgz_SitF
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Optional<Int>
// CHECK:         [[PRECOND:%.*]] = function_ref @$Ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]](
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[WRITE]] : $*Optional<Int>, #Optional.some!enumelt.1
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

// CHECK-LABEL: sil hidden @$S15optional_lvalue011assign_iuo_B9_implicityyAA1SVSgz_SitF
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Optional<S>
// CHECK:         [[SOME:%.*]] = unchecked_take_enum_data_addr [[WRITE]]
// CHECK:         [[X:%.*]] = struct_element_addr [[SOME]]
func assign_iuo_lvalue_implicit(_ s: inout S!, _ y: Int) {
  s.x = y
}

struct Struct<T> {
  var value: T?
}

// CHECK-LABEL: sil hidden @$S15optional_lvalue07assign_a1_B13_reabstractedyyAA6StructVyS2icGz_S2ictF
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$SS2iIegyd_S2iIegnr_TR
// CHECK:         [[REABSTRACTED:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]
// CHECK:         assign [[REABSTRACTED]] to {{%.*}} : $*@callee_guaranteed (@in_guaranteed Int) -> @out Int
func assign_optional_lvalue_reabstracted(_ x: inout Struct<(Int) -> Int>,
                                         _ y: @escaping (Int) -> Int) {
  x.value! = y
}

// CHECK-LABEL: sil hidden @$S15optional_lvalue07assign_a1_B9_computedySiAA1SVSgz_SitF
// CHECK:         function_ref @$S15optional_lvalue1SV8computedSivs
// CHECK:         function_ref @$S15optional_lvalue1SV8computedSivg
func assign_optional_lvalue_computed(_ x: inout S?, _ y: Int) -> Int {
  x!.computed = y
  return x!.computed
}

func generate_int() -> Int { return 0 }

// CHECK-LABEL: sil hidden @$S15optional_lvalue013assign_bound_a1_B0yySiSgzF
// CHECK:         [[HASVALUE:%.*]] = select_enum_addr {{%.*}}
// CHECK:         cond_br [[HASVALUE]], [[SOME:bb[0-9]+]], [[NONE:bb[0-9]+]]
//
// CHECK:       [[SOME]]:
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr
// CHECK:         [[FN:%.*]] = function_ref
// CHECK:         [[T0:%.*]] = apply [[FN]]()
// CHECK:         assign [[T0]] to [[PAYLOAD]]
func assign_bound_optional_lvalue(_ x: inout Int?) {
  x? = generate_int()
}
