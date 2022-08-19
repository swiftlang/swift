// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_do %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// CHECK-LABEL: sil hidden @$s11coverage_do3fooyyF : $@convention(thin) () -> ()

// CHECK:       string_literal
// CHECK-NEXT:  integer_literal $Builtin.Int64, 0
// CHECK-NEXT:  integer_literal $Builtin.Int32, 3
// CHECK-NEXT:  integer_literal $Builtin.Int32, 0
// CHECK-NEXT:  int_instrprof_increment
// CHECK:       function_ref @$sSb6randomSbyFZ
// CHECK:       cond_br {{%[0-9]+}}, [[EXITBB:bb[0-9]]], [[BB1:bb[0-9]]]

// CHECK:       [[BB1]]
// CHECK:       function_ref @$sSb6randomSbyFZ
// CHECK:       cond_br {{%[0-9]+}}, [[BRKBB:bb[0-9]]], {{bb[0-9]}}

// CHECK:       [[BRKBB]]
// CHECK-NEXT:  string_literal
// CHECK-NEXT:  integer_literal $Builtin.Int64, 0
// CHECK-NEXT:  integer_literal $Builtin.Int32, 3
// CHECK-NEXT:  integer_literal $Builtin.Int32, 2

// CHECK:       [[EXITBB]]
// CHECK-NEXT:  string_literal
// CHECK-NEXT:  integer_literal $Builtin.Int64, 0
// CHECK-NEXT:  integer_literal $Builtin.Int32, 3
// CHECK-NEXT:  integer_literal $Builtin.Int32, 1

// CHECK-LABEL: sil_coverage_map {{.*}} "$s11coverage_do3fooyyF"
// CHECK-NEXT:  [[@LINE+11]]:12 -> [[@LINE+18]]:2 : 0
// CHECK-NEXT:  [[@LINE+11]]:9 -> [[@LINE+15]]:4 : 0
// CHECK-NEXT:  [[@LINE+11]]:8 -> [[@LINE+11]]:17 : 0
// CHECK-NEXT:  [[@LINE+10]]:18 -> [[@LINE+10]]:28 : 1
// CHECK-NEXT:  [[@LINE+9]]:28 -> [[@LINE+12]]:4 : (0 - 1)
// CHECK-NEXT:  [[@LINE+9]]:8 -> [[@LINE+9]]:17 : (0 - 1)
// CHECK-NEXT:  [[@LINE+8]]:18 -> [[@LINE+8]]:29 : 2
// CHECK-NEXT:  [[@LINE+7]]:29 -> [[@LINE+8]]:11 : ((0 - 1) - 2)
// CHECK-NEXT:  [[@LINE+8]]:4 -> [[@LINE+10]]:2 : 2
// CHECK-NEXT:  [[@LINE+8]]:6 -> [[@LINE+8]]:8 : 2
// CHECK-NEXT:  [[@LINE+7]]:8 -> [[@LINE+8]]:2 : 2
func foo() {
  x: do {
    if .random() { return }
    if .random() { break x }
    return
  }
  do {}
}
// CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s11coverage_do4foobyyF"
func foob() {
  x: do {
    do {
      if .random() { return }
      // CHECK: [[@LINE+1]]:6 -> [[@LINE+10]]:4 : (0 - 1)
    }
    do {
      if .random() { break x }
      // CHECK: [[@LINE+1]]:6 -> [[@LINE+6]]:4 : ((0 - 1) - 2)
    }
    do {
      return
      // CHECK-NOT: zero
    }
  }
  do {}
}
