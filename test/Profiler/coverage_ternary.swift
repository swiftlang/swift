// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_ternary %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$s16coverage_ternary3barCACycfc
// CHECK-NOT: return
// CHECK: builtin "int_instrprof_increment"

// CHECK-LABEL: sil hidden @$s16coverage_ternary3barCfD
// CHECK-NOT: return
// CHECK: builtin "int_instrprof_increment"

var flag: Int = 0

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_ternary.foo
func foo(_ x : Int32) -> Int32 {
  return x == 3
             ? 9000 // CHECK: [[@LINE]]:16 -> [[@LINE]]:20 : 1
             : 1234 // CHECK: [[@LINE]]:16 -> [[@LINE]]:20 : (0 - 1)
}

foo(1)
foo(2)
foo(3)

// rdar://problem/23256795 - Avoid crash if an if_expr has no parent
// CHECK: sil_coverage_map {{.*}}// __ntd_bar_line:[[@LINE+1]]
class bar {
  var m1 = flag == 0
             ? "false" // CHECK: [[@LINE]]:16 -> [[@LINE]]:23 : 0
             : "true"; // CHECK: [[@LINE]]:16 -> [[@LINE]]:22 : (1 - 0)
}

// Note: We didn't instantiate bar, but we still expect to see instrumentation
// for its *structors, and coverage mapping information for it.
