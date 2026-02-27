// https://github.com/swiftlang/swift/issues/81771
// Verify that in the LLVM IR after CoroSplit, swift_task_dealloc for the second
// async let's closure appears BEFORE swift_asyncLet_finish for the first async
// let, ensuring correct LIFO teardown ordering.

// RUN: %target-swift-frontend -emit-ir -module-name test -swift-version 6 -parse-as-library %s | %FileCheck %s

// REQUIRES: concurrency

struct MyData {
  var p1, p2, p3, p4, p5, p6: Int
  var p7: Int
}

struct MyWrapper: ~Copyable {
  var payload: MyData?
}

func async_let_crash() async {
  async let one: Never? = Never?.none
  async let two: Never? = Never?.none
  if let _ = await one {}
  _ = await two
  _ = try? MyWrapper()
}

// After CoroSplit, the continuation that runs between the two finishAsyncLet
// calls must contain swift_task_dealloc for closure2 BEFORE calling
// swift_asyncLet_finish for closure1.

// CHECK-LABEL: define {{.*}} @"$s4test15async_let_crashyyYaFTY5_"
// CHECK: call swiftcc void @swift_task_dealloc
// CHECK: musttail call swifttailcc void @swift_asyncLet_finish
