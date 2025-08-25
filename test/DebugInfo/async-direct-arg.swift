// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name a  -target %target-swift-5.1-abi-triple \
// RUN:    -parse-as-library | %FileCheck %s --check-prefix=CHECK
// REQUIRES: concurrency
// REQUIRES: CPU=x86_64 || CPU=arm64


// Test that x is described as a direct dbg.declare of the incoming function
// argument.

// CHECK-LABEL: define {{.*}} void @"$s1a3fibyS2iYaFTY0_"
// CHECK-SAME:    (ptr swiftasync %[[ARG:.*]])
// CHECK: #dbg_declare(ptr %[[ARG]], {{.*}}, !DIExpression(DW_OP_LLVM_entry_value
// CHECK: #dbg_declare(ptr %[[ARG]], {{.*}}, !DIExpression(DW_OP_LLVM_entry_value
// CHECK: #dbg_declare(ptr %[[ARG]], ![[X0:[0-9]+]], !DIExpression(DW_OP_LLVM_entry_value

// CHECK-LABEL: define {{.*}} void @"$s1a3fibyS2iYaFTY2_"
// CHECK-SAME:    (ptr swiftasync %[[ARG:.*]])
// CHECK: #dbg_declare(ptr %[[ARG]], {{.*}}, !DIExpression(DW_OP_LLVM_entry_value
// CHECK: #dbg_declare(ptr %[[ARG]], {{.*}}, !DIExpression(DW_OP_LLVM_entry_value
// CHECK: #dbg_declare(ptr %[[ARG]], ![[X1:[0-9]+]], !DIExpression(DW_OP_LLVM_entry_value

// CHECK: ![[X0]] = !DILocalVariable(name: "x"
// CHECK: ![[X1]] = !DILocalVariable(name: "x"
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
