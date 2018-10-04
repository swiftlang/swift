
// RUN: %target-swift-emit-silgen -module-name optional -enable-sil-ownership %s | %FileCheck %s

func testCall(_ f: (() -> ())?) {
  f?()
}
// CHECK:    sil hidden @{{.*}}testCall{{.*}}
// CHECK:    bb0([[T0:%.*]] : @guaranteed $Optional<@callee_guaranteed () -> ()>):
// CHECK:      [[T0_COPY:%.*]] = copy_value [[T0]]
// CHECK-NEXT: switch_enum [[T0_COPY]] : $Optional<@callee_guaranteed () -> ()>, case #Optional.some!enumelt.1: [[SOME:bb[0-9]+]], case #Optional.none!enumelt: [[NONE:bb[0-9]+]]
//
// CHECK: [[NONE]]:
// CHECK:   br [[NOTHING_BLOCK_EXIT:bb[0-9]+]]

//   If it does, project and load the value out of the implicitly unwrapped
//   optional...

// CHECK: [[SOME]]([[FN0:%.*]] :
//   .... then call it
// CHECK-NEXT: [[B:%.*]] = begin_borrow [[FN0]]
// CHECK-NEXT: apply [[B]]()
// CHECK:      destroy_value [[FN0]]
// CHECK:      br [[EXIT:bb[0-9]+]](

//   (first nothing block)
// CHECK:    [[NOTHING_BLOCK_EXIT]]:
// CHECK-NEXT: enum $Optional<()>, #Optional.none!enumelt
// CHECK-NEXT: br [[EXIT]]
// CHECK: } // end sil function '$s8optional8testCallyyyycSgF'

func testAddrOnlyCallResult<T>(_ f: (() -> T)?) {
  var f = f
  var x = f?()
}
// CHECK-LABEL: sil hidden @{{.*}}testAddrOnlyCallResult{{.*}} : $@convention(thin) <T> (@guaranteed Optional<@callee_guaranteed () -> @out T>) -> ()
// CHECK:    bb0([[T0:%.*]] : @guaranteed $Optional<@callee_guaranteed () -> @out T>):
// CHECK: [[F:%.*]] = alloc_box $<τ_0_0> { var Optional<@callee_guaranteed () -> @out τ_0_0> } <T>, var, name "f"
// CHECK-NEXT: [[PBF:%.*]] = project_box [[F]]
// CHECK: [[T0_COPY:%.*]] = copy_value [[T0]]
// CHECK: store [[T0_COPY]] to [init] [[PBF]]
// CHECK-NEXT: [[X:%.*]] = alloc_box $<τ_0_0> { var Optional<τ_0_0> } <T>, var, name "x"
// CHECK-NEXT: [[PBX:%.*]] = project_box [[X]]
// CHECK-NEXT: [[TEMP:%.*]] = init_enum_data_addr [[PBX]]
// CHECK-NEXT: [[READ:%.*]] = begin_access [read] [unknown] [[PBF]]
//   Check whether 'f' holds a value.
// CHECK: [[HASVALUE:%.*]] = select_enum_addr [[READ]]
// CHECK-NEXT: cond_br [[HASVALUE]], bb2, bb1
//   If so, pull out the value...
// CHECK:    bb2:
// CHECK-NEXT: [[T1:%.*]] = unchecked_take_enum_data_addr [[READ]]
// CHECK-NEXT: [[T0:%.*]] = load [copy] [[T1]]
// CHECK-NEXT: end_access [[READ]]
//   ...evaluate the rest of the suffix...
// CHECK:     [[B:%.*]] = begin_borrow [[T0]]
// CHECK-NEXT: apply [[B]]([[TEMP]])
//   ...and coerce to T?
// CHECK: inject_enum_addr [[PBX]] {{.*}}some
// CHECK:     destroy_value [[T0]]
// CHECK-NEXT: br bb3
//   Continuation block.
// CHECK:    bb3
// CHECK-NEXT: destroy_value [[X]]
// CHECK-NEXT: destroy_value [[F]]
// CHECK-NOT: destroy_value %0
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]] : $()

//   Nothing block.
// CHECK:    bb4:
// CHECK-NEXT: inject_enum_addr [[PBX]] {{.*}}none
// CHECK-NEXT: br bb3


// <rdar://problem/15180622>

func wrap<T>(_ x: T) -> T? { return x }

// CHECK-LABEL: sil hidden @$s8optional16wrap_then_unwrap{{[_0-9a-zA-Z]*}}F
func wrap_then_unwrap<T>(_ x: T) -> T {
  // CHECK:   switch_enum_addr {{.*}}, case #Optional.some!enumelt.1: [[OK:bb[0-9]+]], case #Optional.none!enumelt: [[FAIL:bb[0-9]+]]
  // CHECK: [[FAIL]]:
  // CHECK:   unreachable
  // CHECK: [[OK]]:
  // CHECK:   unchecked_take_enum_data_addr
  return wrap(x)!
}

// CHECK-LABEL: sil hidden @$s8optional10tuple_bind{{[_0-9a-zA-Z]*}}F
func tuple_bind(_ x: (Int, String)?) -> String? {
  return x?.1
  // CHECK:   switch_enum {{%.*}}, case #Optional.some!enumelt.1: [[NONNULL:bb[0-9]+]], case #Optional.none!enumelt: [[NULL:bb[0-9]+]]
  // CHECK: [[NONNULL]]([[TUPLE:%.*]] :
  // CHECK:   ({{%.*}}, [[STRING:%.*]]) = destructure_tuple [[TUPLE]]
  // CHECK:   [[RESULT:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[STRING]]
  // CHECK-NOT:   destroy_value [[STRING]]
  // CHECK:   br {{bb[0-9]+}}([[RESULT]] :
}

// rdar://21883752 - We were crashing on this function because the deallocation happened
// out of scope.
// CHECK-LABEL: sil hidden @$s8optional16crash_on_deallocyySDySiSaySiGGFfA_
func crash_on_dealloc(_ dict : [Int : [Int]] = [:]) {
  var dict = dict
  dict[1]?.append(2)
}

func use_unwrapped(_: Int) {}

// CHECK-LABEL: sil hidden @$s8optional15explicit_unwrap{{[_0-9a-zA-Z]*}}F
// CHECK:         [[FILESTR:%.*]] = string_literal utf8 "{{.*}}optional.swift"
// CHECK-NEXT:         [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:         [[LINE:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:         [[PRECOND:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
func explicit_unwrap(_ value: Int?) {
  use_unwrapped(value!)
}

// CHECK-LABEL: sil hidden @$s8optional19explicit_iuo_unwrap{{[_0-9a-zA-Z]*}}F
// CHECK:         [[FILESTR:%.*]] = string_literal utf8 "{{.*}}optional.swift"
// CHECK-NEXT:         [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:         [[LINE:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:         [[PRECOND:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
func explicit_iuo_unwrap(_ value: Int!) {
  use_unwrapped(value!)
}

// CHECK-LABEL: sil hidden @$s8optional19implicit_iuo_unwrap{{[_0-9a-zA-Z]*}}F
// CHECK:         [[FILESTR:%.*]] = string_literal utf8 "{{.*}}optional.swift"
// CHECK-NEXT:         [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:         [[LINE:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:         [[PRECOND:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
func implicit_iuo_unwrap(_ value: Int!) {
  use_unwrapped(value)
}

// CHECK-LABEL: sil hidden @$s8optional34implicit_iuo_unwrap_sourceLocation{{[_0-9a-zA-Z]*}}F
// CHECK:         [[FILESTR:%.*]] = string_literal utf8 "custom.swuft"
// CHECK-NEXT:         [[FILESIZ:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[FILEASC:%.*]] = integer_literal $Builtin.Int1, 
// CHECK-NEXT:         [[LINE:%.*]] = integer_literal $Builtin.Word, 2000
// CHECK-NEXT:         [[COLUMN:%.*]] = integer_literal $Builtin.Word, 
// CHECK-NEXT:         [[IMPLICIT:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:         [[PRECOND:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[PRECOND]]([[FILESTR]], [[FILESIZ]], [[FILEASC]], [[LINE]], [[IMPLICIT]])
func implicit_iuo_unwrap_sourceLocation(_ value: Int!) {
#sourceLocation(file: "custom.swuft", line: 2000)
  use_unwrapped(value)
#sourceLocation() // reset
}
