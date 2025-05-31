
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name optional_lvalue %s | %FileCheck %s

// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue07assign_a1_B0yySiSgz_SitF
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Optional<Int>
// CHECK:         [[FILESTR:%.*]] = string_literal utf8 "
// CHECK-NEXT:         [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:         [[LINE:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:         [[PRECOND:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[WRITE]] : $*Optional<Int>, #Optional.some!enumelt
// CHECK:         assign {{%.*}} to [[PAYLOAD]]
func assign_optional_lvalue(_ x: inout Int?, _ y: Int) {
  x! = y
}

// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue011assign_iuo_B0yySiSgz_SitF
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Optional<Int>
// CHECK:         [[FILESTR:%.*]] = string_literal utf8 "
// CHECK-NEXT:         [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:         [[LINE:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:         [[PRECOND:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
// CHECK:         [[PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[WRITE]] : $*Optional<Int>, #Optional.some!enumelt
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

// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue011assign_iuo_B9_implicityyAA1SVSgz_SitF
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Optional<S>
// CHECK:         [[FILESTR:%.*]] = string_literal utf8 "
// CHECK-NEXT:         [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:         [[LINE:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:         [[PRECOND:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
// CHECK:         [[SOME:%.*]] = unchecked_take_enum_data_addr [[WRITE]]
// CHECK:         [[X:%.*]] = struct_element_addr [[SOME]]
func assign_iuo_lvalue_implicit(_ s: inout S!, _ y: Int) {
  s.x = y
}

struct Struct<T> {
  var value: T?
}

// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue07assign_a1_B13_reabstractedyyAA6StructVyS2icGz_S2ictF
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$sS2iIegyd_S2iIegnr_TR
// CHECK:         [[REABSTRACTED:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]
// CHECK:         [[REABSTRACTED_CONV:%.*]] = convert_function [[REABSTRACTED]]
// CHECK:         assign [[REABSTRACTED_CONV]] to {{%.*}} :
func assign_optional_lvalue_reabstracted(_ x: inout Struct<(Int) -> Int>,
                                         _ y: @escaping (Int) -> Int) {
  x.value! = y
}

// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue07assign_a1_B9_computedySiAA1SVSgz_SitF
// CHECK:         function_ref @$s15optional_lvalue1SV8computedSivs
// CHECK:         function_ref @$s15optional_lvalue1SV8computedSivg
func assign_optional_lvalue_computed(_ x: inout S?, _ y: Int) -> Int {
  x!.computed = y
  return x!.computed
}

func generate_int() -> Int { return 0 }

// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue013assign_bound_a1_B0yySiSgzF
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

struct ComputedOptional {
  var computedOptional : Int? {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue013assign_bound_a10_computed_B0yyAA16ComputedOptionalVzF
// CHECK:  [[SELF:%.*]] = begin_access [modify] [unknown] %0 : $*ComputedOptional
// CHECK:  [[TEMP:%.*]] = alloc_stack $Optional<Int>
// CHECK:  [[T0:%.*]] = load [trivial] [[SELF]] : $*ComputedOptional
// CHECK:  [[GETTER:%.*]] = function_ref @$s15optional_lvalue16ComputedOptionalV08computedD0SiSgvg
// CHECK:  [[VALUE:%.*]] = apply [[GETTER]]([[T0]])
// CHECK:  store [[VALUE]] to [trivial] [[TEMP]] : $*Optional<Int>
// CHECK:  select_enum_addr [[TEMP]] : $*Optional<Int>
// CHECK:  cond_br
// CHECK:  [[VALUE_ADDR:%.*]] = unchecked_take_enum_data_addr [[TEMP]] : $*Optional<Int>
// CHECK:  [[GENERATOR:%.*]] = function_ref @$s15optional_lvalue12generate_intSiyF
// CHECK:  [[VALUE:%.*]] = apply [[GENERATOR]]()
// CHECK:  assign [[VALUE]] to [[VALUE_ADDR]] : $*Int
// CHECK:  [[OPTVALUE:%.*]] = load [trivial] [[TEMP]] : $*Optional<Int>
// CHECK:  [[SETTER:%.*]] = function_ref @$s15optional_lvalue16ComputedOptionalV08computedD0SiSgvs
// CHECK:  apply [[SETTER]]([[OPTVALUE]], [[SELF]])
// CHECK:  end_access [[SELF]]
// CHECK:  dealloc_stack [[TEMP]]
func assign_bound_optional_computed_lvalue(_ co: inout ComputedOptional) {
  co.computedOptional? = generate_int()
}

// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue014assign_forced_a10_computed_B0yyAA16ComputedOptionalVzF
// CHECK:  [[GENERATOR:%.*]] = function_ref @$s15optional_lvalue12generate_intSiyF
// CHECK:  [[VALUE:%.*]] = apply [[GENERATOR]]()
// CHECK:  [[SELF:%.*]] = begin_access [modify] [unknown] %0 : $*ComputedOptional
// CHECK:  [[TEMP:%.*]] = alloc_stack $Optional<Int>
// CHECK:  [[T0:%.*]] = load [trivial] [[SELF]] : $*ComputedOptional
// CHECK:  [[GETTER:%.*]] = function_ref @$s15optional_lvalue16ComputedOptionalV08computedD0SiSgvg
// CHECK:  [[OPTVALUE:%.*]] = apply [[GETTER]]([[T0]])
// CHECK:  store [[OPTVALUE]] to [trivial] [[TEMP]]
// CHECK:  switch_enum_addr [[TEMP]]
// CHECK:  [[VALUE_ADDR:%.*]] = unchecked_take_enum_data_addr [[TEMP]] : $*Optional<Int>
// CHECK:  assign [[VALUE]] to [[VALUE_ADDR]] : $*Int
// CHECK:  [[OPTVALUE:%.*]] = load [trivial] [[TEMP]] : $*Optional<Int>
// CHECK:  [[SETTER:%.*]] = function_ref @$s15optional_lvalue16ComputedOptionalV08computedD0SiSgvs
// CHECK:  apply [[SETTER]]([[OPTVALUE]], [[SELF]])
// CHECK:  end_access [[SELF]]
// CHECK:  dealloc_stack [[TEMP]]
func assign_forced_optional_computed_lvalue(_ co: inout ComputedOptional) {
  co.computedOptional! = generate_int()
}

// Make sure we load the ignored lvalues here to evaluate the side effects.
// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue17testIgnoredLValueyyF : $@convention(thin) () -> ()
func testIgnoredLValue() {
  var x: Int?
  x!
  // CHECK: function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
  // CHECK: unreachable

  // CHECK-LABEL: sil private [ossa] @$s15optional_lvalue17testIgnoredLValueyyFyycfU_ : $@convention(thin) (@guaranteed { var Optional<Int> }) -> ()
  let _: () -> Void = {
    x!
    // CHECK: function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
    // CHECK: unreachable
  }
  // CHECK-LABEL: sil private [ossa] @$s15optional_lvalue17testIgnoredLValueyyFyycfU0_ : $@convention(thin) (@guaranteed { var Optional<Int> }) -> ()
  let _: () -> Void = {
    x!
    // CHECK: function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
    // CHECK: unreachable
    ()
  }
}

class C {}

// We don't need to emit a load here.
// CHECK-LABEL: sil hidden [ossa] @$s15optional_lvalue16testNoLoadLValueyyF : $@convention(thin) () -> ()
func testNoLoadLValue() {
  var x: C?
  x
  // CHECK:      [[BOX:%[0-9]+]] = alloc_box ${ var Optional<C> }, var, name "x"
  // CHECK-NOT:  load
  // CHECK:      destroy_value [[BOX]]
}
