// RUN: %target-swift-frontend -O -emit-sil -primary-file %s | FileCheck %s
//
// Test Array "make_mutable" hoisting.  It's hard for FileCheck to
// recognize the hoisting because we don't know which block is the
// loop header. If this becomes to fragile, we could check debug-only
// output instead.

// CHECK-LABEL: sil hidden @_TF13array_mutable8inoutarrFRGSaSi_T_
// CHECK: %[[FR:[0-9]+]] = function_ref @_swift_isUniquelyReferenced
// CHECK-NOT: {{^bb}}
// CHECK: apply %[[FR]]
// CHECK: {{^bb}}
// CHECK-NOT: _swift_isUniquelyReferenced
// CHECK: [[VOID:%[^ ]+]] = tuple ()
// CHECK: return [[VOID]]
func inoutarr(inout a: [Int]) {
  for i in 0..<a.count {
    a[i] = 0
  }
}

struct S {
  var a: [Int]
}

// CHECK-LABEL: sil hidden @_TF13array_mutable6arreltFRVS_1ST_
// CHECK: %[[FR:[0-9]+]] = function_ref @_swift_isUniquelyReferenced
// CHECK-NOT: {{^bb}}
// CHECK: apply %[[FR]]
// CHECK: {{^bb}}
// CHECK-NOT: _swift_isUniquelyReferenced
// CHECK: {{^[}]}}
func arrelt(inout s: S) {
  for i in 0..<s.a.count {
    s.a[i] = 0
  }
}

class ArrayInClass {
  final var A : [Int]
  final var B : [Int]
  final var C : [[Int]]

  init() {
    A = []
    B = []
    C = [[]]
  }

  // CHECK-LABEL: sil hidden @_TFC13array_mutable12ArrayInClass12hoistInClassfS0_FT_T_
  // CHECK: %[[FR:[0-9]+]] = function_ref @_swift_isUniquelyReferenced
  // CHECK-NOT: {{^bb}}
  // CHECK: apply %[[FR]]
  // CHECK: {{^bb}}
  // CHECK-NOT: _swift_isUniquelyReferenced
  // CHECK: {{^[}]}}

  func hoistInClass() {
    for i in 0..<A.count {
      A[i] = 0
    }
  }
  // CHECK-LABEL: sil hidden @_TFC13array_mutable12ArrayInClass16hoistInClass2ArrfS0_FT_T_
  // CHECK: %[[FR:[0-9]+]] = function_ref @_swift_isUniquelyReferenced
  // CHECK-NOT: {{^bb}}
  // CHECK: apply %[[FR]]
  // CHECK: {{^bb}}
  // CHECK: apply %[[FR]]
  // CHECK: {{^bb}}
  // CHECK-NOT: _swift_isUniquelyReferenced
  // CHECK: {{^[}]}}
  func hoistInClass2Arr() {
    for i in 0..<A.count {
      A[i] = 0
      B[i] = 2
    }
  }

  // Temporary disable.
  // DISABLEDCHECK-LABEL: sil hidden @_TFC13array_mutable12ArrayInClass22dontHoistInClassAppendfS0_FT_T_
  // DISABLEDCHECK: %[[FR:[0-9]+]] = function_ref @_swift_isUniquelyReferenced
  // DISABLEDCHECK-NOT: apply %[[FR]]
  // DISABLEDCHECK: {{^bb}}
  // DISABLEDCHECK: apply %[[FR]]
  // DISABLEDCHECK: {{^[}]}}
  func dontHoistInClassAppend() {
    for i in 0..<A.count {
      A[i] = 0
      C.append(A)
    }
  }
}
