// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name a  -target %target-swift-5.1-abi-triple \
// RUN:    | %FileCheck %s --check-prefix=CHECK
// REQUIRES: concurrency

// Test that lifetime extension preserves a dbg.declare for "n" in the resume
// funclet.

// CHECK-LABEL: define {{.*}} void @"$s1a4fiboyS2iYaFTY0_"
// CHECK-NEXT: entryresume.0:
// CHECK-NEXT: #dbg_declare({{.*}}%0, ![[RHS:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-NEXT: #dbg_declare({{.*}}%0, ![[LHS:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-NEXT: #dbg_declare({{.*}}%0, ![[R:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-NEXT: #dbg_declare({{.*}}%0, ![[N:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-NOT: {{ ret }}
// CHECK: call void asm sideeffect ""
// CHECK: ![[N]] = !DILocalVariable(name: "n"
// CHECK: ![[R]] = !DILocalVariable(name: "retval"
// CHECK: ![[LHS]] = !DILocalVariable(name: "lhs"
// CHECK: ![[RHS]] = !DILocalVariable(name: "rhs"
public func fibo(_ n: Int) async -> Int {
  var retval = n
  if retval < 2 { return 1 }
  retval = retval - 1
  let lhs = await fibo(retval - 1)
  let rhs = await fibo(retval - 2)
  return lhs + rhs + retval
}
