// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -primary-file %s | %FileCheck %s

// Tests for the ForEachLoopUnroll mandatory optimization pass that unrolls
// Sequence.forEach calls over array literals.

// CHECK-LABEL: sil hidden @$s25for_each_loop_unroll_test0D19LetArrayLiteralTestyyF : $@convention(thin) () -> ()
func unrollLetArrayLiteralTest() {
  let a = [Int64(15), Int64(27)]
  a.forEach { print($0) }
  // CHECK: [[LIT1:%[0-9]+]] = integer_literal $Builtin.Int64, 15
  // CHECK: [[INT1:%[0-9]+]] = struct $Int64 ([[LIT1]] : $Builtin.Int64)
  // CHECK: [[LIT2:%[0-9]+]] = integer_literal $Builtin.Int64, 27
  // CHECK: [[INT2:%[0-9]+]] = struct $Int64 ([[LIT2]] : $Builtin.Int64)
  // CHECK-NOT: forEach
  // CHECK: [[STACK:%[0-9]+]] = alloc_stack $Int64
  // CHECK: store [[INT1]] to [[STACK]]
  // CHECK: try_apply %{{.*}}([[STACK]]) : {{.*}}, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]

  // CHECK: [[NORMAL]](%{{.*}} : $()):
  // CHECK: store [[INT2]] to [[STACK]] : $*Int64
  // CHECK: try_apply {{.*}}([[STACK]])
}

// CHECK-LABEL: sil hidden @$s25for_each_loop_unroll_test0D35LetArrayLiteralWithVariableElements1x1yys5Int64V_AFtF
func unrollLetArrayLiteralWithVariableElements(x: Int64, y: Int64) {
  let a = [x, y]
  a.forEach { print($0) }
  // CHECK-NOT: forEach
  // CHECK: [[STACK:%[0-9]+]] = alloc_stack $Int64
  // CHECK: store %0 to [[STACK]]
  // CHECK: try_apply %{{.*}}([[STACK]]) : {{.*}}, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
  
  // CHECK: [[NORMAL]](%{{.*}} : $()):
  // CHECK: store %1 to [[STACK]] : $*Int64
  // CHECK: try_apply {{.*}}([[STACK]])
}

// CHECK-LABEL: sil hidden @$s25for_each_loop_unroll_test0D37LetArrayLiteralWithNonTrivialElementsyyF
func unrollLetArrayLiteralWithNonTrivialElements() {
  let a = ["a", "aa"]
  a.forEach { print($0) }
  // CHECK: [[LIT1:%[0-9]+]] = string_literal utf8 "a"
  // CHECK: [[STRING_INIT:%[0-9]+]] = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC
  // CHECK: [[STRING1:%[0-9]+]] = apply [[STRING_INIT]]([[LIT1]],
  
  // CHECK: [[LIT2:%[0-9]+]] = string_literal utf8 "aa"
  // CHECK: [[STRING_INIT2:%[0-9]+]] = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC
  // CHECK: [[STRING2:%[0-9]+]] = apply [[STRING_INIT2]]([[LIT2]],

  // CHECK-NOT: forEach
  // CHECK: [[STACK:%[0-9]+]] = alloc_stack $String
  // CHECK: store [[STRING1]] to [[STACK]] : $*String
  // CHECK: try_apply %{{.*}}([[STACK]]) : {{.*}}, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
  
  // CHECK: [[NORMAL]](%{{.*}} : $()):
  // CHECK: store [[STRING2]] to [[STACK]] : $*String
  // CHECK: try_apply {{.*}}([[STACK]])
}

// This test mimics the array literal and forEach created by the OSLogOptimization pass.
// CHECK-LABEL: sil hidden @$s25for_each_loop_unroll_test0D27LetArrayLiteralWithClosures1i1jys5Int32V_AFtF
func unrollLetArrayLiteralWithClosures(i: Int32, j: Int32) {
  let a = [{ i } , { j }]
  a.forEach { print($0()) }
  // CHECK: [[ALLOCATE:%[0-9]+]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
  // CHECK: [[ARRAYTUP:%[0-9]+]] = apply [[ALLOCATE]]<() -> Int32>
  // CHECK: [[ARRAYVAL:%[0-9]+]] =  tuple_extract [[ARRAYTUP]] : $(Array<() -> Int32>, Builtin.RawPointer), 0
  // CHECK: [[STORAGEPTR:%[0-9]+]] =  tuple_extract [[ARRAYTUP]] : $(Array<() -> Int32>, Builtin.RawPointer), 1
  // CHECK: [[MDI:%[0-9]+]] = mark_dependence [[STORAGEPTR]] : $Builtin.RawPointer on [[ARRAYVAL]] : $Array<() -> Int32>
  // CHECK: [[STORAGEADDR:%[0-9]+]] = pointer_to_address [[MDI]]
  // CHECK: store [[CLOSURE1:%[0-9]+]] to [[STORAGEADDR]]
  // CHECK: [[INDEX1:%[0-9]+]] = index_addr [[STORAGEADDR]]
  // CHECK: store [[CLOSURE2:%[0-9]+]] to [[INDEX1]]
  
  // CHECK-NOT: forEach
  // CHECK: [[STACK:%[0-9]+]] = alloc_stack
  // CHECK: store [[CLOSURE1]] to [[STACK]]
  // CHECK: try_apply %{{.*}}([[STACK]]) : ${{.*}}, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
  
  // CHECK: [[NORMAL]](%{{.*}} : $()):
  // CHECK: store [[CLOSURE2]] to [[STACK]]
  // CHECK: try_apply {{.*}}([[STACK]])
}

// CHECK-LABEL: sil hidden @$s25for_each_loop_unroll_test0E16NoUnrollScenarioyyF
func testNoUnrollScenario() {
  var a = [Int64(15), Int64(27)]
  a.append(Int64(7))
  a.forEach { print($0) }
  // CHECK: forEach
}

// FIXME: Currently, array literals with address-only types cannot be unrolled
// as they are initialized using copy_addr instead of store.
// CHECK-LABEL: sil hidden @$s25for_each_loop_unroll_test0E27UnrollingOfAddressOnlyTypes1x1yyx_xtlF
func testUnrollingOfAddressOnlyTypes<T>(x: T, y: T) {
  let a = [x, y]
  a.forEach { print($0) }
  // CHECK: forEach
}

// Check that when there are multiple forEach loops they are unrolled.
// CHECK-LABEL: sil hidden @$s25for_each_loop_unroll_test0D13MultipleLoops1x1y1zys5Int64V_AGSbtF
func unrollMultipleLoops(x: Int64, y: Int64, z: Bool) {
  let a = [x, y]
  if z {
    a.forEach { print($0) }
  } else {
    a.forEach{ print($0 + 1) }
  }
    // CHECK-NOT: forEach
    // CHECK-LABEL: end sil function '$s25for_each_loop_unroll_test0D13MultipleLoops1x1y1zys5Int64V_AGSbtF'
}
