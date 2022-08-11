// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_deinit %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

func foo() {}

public class Foo {
  // CHECK-LABEL: sil @$s15coverage_deinit3FooCfd
  // CHECK:       integer_literal $Builtin.Int64, 0
  // CHECK:       integer_literal $Builtin.Int32, 2
  // CHECK:       integer_literal $Builtin.Int32, 0
  // CHECK-NEXT:  builtin "int_instrprof_increment"
  // CHECK:       function_ref @$sSb6randomSbyFZ
  // CHECK:       cond_br
  // CHECK:       integer_literal $Builtin.Int64, 0
  // CHECK:       integer_literal $Builtin.Int32, 2
  // CHECK:       integer_literal $Builtin.Int32, 1
  // CHECK-NEXT:  builtin "int_instrprof_increment"
  // CHECK-NEXT:  // function_ref coverage_deinit.foo() -> ()
  deinit {
    if .random() {
      foo()
    }
  }
}

// CHECK-LABEL: sil_coverage_map "{{.*}}coverage_deinit.swift" "$s15coverage_deinit3FooCfd"
// CHECK-NEXT: [[@LINE-8]]:10 -> [[@LINE-4]]:4 : 0
// CHECK-NEXT: [[@LINE-8]]:8 -> [[@LINE-8]]:17 : 0
// CHECK-NEXT: [[@LINE-9]]:18 -> [[@LINE-7]]:6 : 1
// CHECK-NEXT: [[@LINE-8]]:6 -> [[@LINE-7]]:4 : 0
