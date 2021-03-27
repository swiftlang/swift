// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name a -enable-experimental-concurrency \
// RUN:    -parse-as-library | %FileCheck %s --check-prefix=CHECK
// REQUIRES: concurrency

// REQUIRES: rdar74588568

// Test that x is described as a direct dbg.declare of the incoming function
// argument.

// CHECK-LABEL: define {{.*}} void @"$s1a3fibyS2iYF.resume.0"
// CHECK: call void @llvm.dbg.declare
// CHECK: call void @llvm.dbg.declare
// CHECK: call void @llvm.dbg.declare(metadata {{.*}}%0, metadata ![[X0:[0-9]+]], {{.*}}!DIExpression(DW_OP
// CHECK-LABEL: define {{.*}} void @"$s1a3fibyS2iYF.resume.1"
// FIXME: call void @llvm.dbg.declare(metadata {{.*}}%0, metadata ![[X1:[0-9]+]], {{.*}}!DIExpression(DW_OP

// CHECK: ![[X0]] = !DILocalVariable(name: "x"
// FIXME: ![[X1]] = !DILocalVariable(name: "x"
func fib(_ x: Int) async -> Int {
  if x <= 1 { return 1 }
  let a = await fib(x - 1)
  let b = await fib(x - 2)
  return a + b
}

@main struct Main {
  static func main() async {
    await fib(4)
  }
}
