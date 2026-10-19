// RUN: %target-swift-frontend -emit-sil -O -enable-sil-opaque-values -sil-verify-all -parse-as-library -module-name test %s | %FileCheck %s

// Deleting unused storage during address lowering must leave an address-typed
// undef in the debug value for an address-only variable.

// CHECK-LABEL: sil [noinline] @{{.*}}4size{{.*}} : $@convention(thin) <T> (@in_guaranteed T) -> Int {
// CHECK: debug_value {{.*}}undef : $*T, let, name "value", argno 1, expr op_deref
// CHECK: return
@inline(never)
public func size<T>(_ value: borrowing T) -> Int {
  MemoryLayout<T>.size
}
