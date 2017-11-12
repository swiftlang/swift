// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_if %s | %FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_if.foo
func foo(x : Bool) { // CHECK: [[@LINE]]:20 -> {{[0-9]+}}:2 : 0
  if (x) { // CHECK: [[@LINE]]:10 -> [[@LINE+1]]:4 : 1
  }

  if (!x) { // CHECK: [[@LINE]]:11 -> [[@LINE+2]]:4 : 2
    // ...
  }

  if (x) { } // CHECK: [[@LINE]]:10 -> [[@LINE]]:13 : 3
  else   { } // CHECK: [[@LINE]]:10 -> [[@LINE]]:13 : (0 - 3)
}

foo(x: true);
foo(x: false);
