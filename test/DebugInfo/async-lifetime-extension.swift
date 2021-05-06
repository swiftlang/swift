// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name a -enable-experimental-concurrency \
// RUN:    | %FileCheck %s --check-prefix=CHECK
// REQUIRES: concurrency

// UNSUPPORTED: CPU=arm64e

// Test that lifetime extension preserves a dbg.declare for "n" in the resume
// funclet.

// CHECK-LABEL: define {{.*}} void @"$s1a4fiboyS2iYaFTQ0_"
// CHECK-NEXT: entryresume.0:
// CHECK-NEXT: call void @llvm.dbg.declare(metadata {{.*}}%0, metadata ![[RHS:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-NEXT: call void @llvm.dbg.declare(metadata {{.*}}%0, metadata ![[LHS:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-NEXT: call void @llvm.dbg.declare(metadata {{.*}}%0, metadata ![[N:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-NEXT: call void @llvm.dbg.declare(metadata {{.*}}%0, metadata ![[R:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-NOT: {{ ret }}
// CHECK: call void asm sideeffect ""
// CHECK: ![[RHS]] = !DILocalVariable(name: "rhs"
// CHECK: ![[LHS]] = !DILocalVariable(name: "lhs"
// CHECK: ![[N]] = !DILocalVariable(name: "n"
// CHECK: ![[R]] = !DILocalVariable(name: "retval"
public func fibo(_ n: Int) async -> Int {
  var retval = n
  if retval < 2 { return 1 }
  retval = retval - 1
  let lhs = await fibo(retval - 1)
  let rhs = await fibo(retval - 2)
  return lhs + rhs + retval
}
