// REQUIRES: optimized_stdlib

// RUN: %target-swift-frontend -emit-sil -o /dev/null \
// RUN:   %s -Xllvm -sil-print-debuginfo -Onone -sil-verify-all \
// RUN:   -Xllvm -sil-print-after=diagnostic-constant-propagation \
// RUN:   2>&1 | %FileCheck %s

// CHECK: alloc_stack $R, loc {{.*}}, scope 2
// CHECK-NEXT: init_existential_addr {{.*}} : $*R, $Float, loc {{.*}}, scope 2
// CHECK-NEXT: copy_addr [take] %8 to [initialization] {{.*}} : $*Float, loc {{.*}}, scope 2

protocol R {}
extension Float: R {}
print(1.0 as Float? as! R)
