// RUN: %target-swift-frontend -emit-sil %s | %FileCheck -check-prefix=CHECK-VJP %s
// RUN: %target-swift-frontend -emit-sil -Xllvm -differentiation-use-vjp=false %s | %FileCheck -check-prefix=CHECK-NOVJP %s

@inline(never)
func print<T>(_ x: T) {
  Swift.print(x)
}

func squared(_ x: Float) -> Float {
  print("test output")
  return x * x
}

_ = gradient(at: 20, in: squared)

// CHECK-VJP-LABEL: sil hidden @{{.*}}squared{{.*}}__primal_src_0_wrt_0
// CHECK-VJP: [[PV:%.*]] = struct ${{.*}}squared{{.*}}__Type__src_0_wrt_0 ({{.*}} : $Float, {{.*}} : $@callee_guaranteed (Float) -> (Float, Float))
// CHECK-VJP: [[RESULT:%.*]] = tuple ([[PV]] : ${{.*}}squared{{.*}}__Type__src_0_wrt_0, {{.*}} : $Float)
// CHECK-VJP: return [[RESULT]] : $({{.*}}squared{{.*}}__Type__src_0_wrt_0, Float)

// CHECK-NOVJP-LABEL: sil hidden @{{.*}}squared{{.*}}__primal_src_0_wrt_0
// CHECK-NOVJP: [[PV:%.*]] = struct ${{.*}}squared{{.*}}__Type__src_0_wrt_0 ({{.*}} : $Float)
// CHECK-NOVJP: [[RESULT:%.*]] = tuple ([[PV]] : ${{.*}}squared{{.*}}__Type__src_0_wrt_0, {{.*}} : $Float)
// CHECK-NOVJP: return [[RESULT]] : $({{.*}}squared{{.*}}__Type__src_0_wrt_0, Float)
