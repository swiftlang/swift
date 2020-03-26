// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_ternary %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// coverage_ternary.bar.init() -> coverage_ternary.bar
// CHECK-LABEL: sil hidden @$s16coverage_ternary3barCACycfc
// CHECK-NOT: return
// CHECK: builtin "int_instrprof_increment"

// rdar://problem/23256795 - Avoid crash if an if_expr has no parent
// CHECK: sil_coverage_map {{.*}}// variable initialization expression of coverage_ternary.bar.m1 : Swift.String
class bar {
  var m1 = flag == 0   // CHECK: [[@LINE]]:12 -> [[@LINE+2]]:22 : 0
             ? "false" // CHECK: [[@LINE]]:16 -> [[@LINE]]:23 : 1
             : "true"; // CHECK: [[@LINE]]:16 -> [[@LINE]]:22 : (0 - 1)
}

// Note: We didn't instantiate bar, but we still expect to see instrumentation
// for its *structors, and coverage mapping information for it.

var flag: Int = 0

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_ternary.foo
func foo(_ x : Int32) -> Int32 { // CHECK: [[@LINE]]:32 -> [[@LINE+4]]:2 : 0
  return x == 3
             ? 9000 // CHECK: [[@LINE]]:16 -> [[@LINE]]:20 : 1
             : 1234 // CHECK: [[@LINE]]:16 -> [[@LINE]]:20 : (0 - 1)
}

foo(1)
foo(2)
foo(3)
