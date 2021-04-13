// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name a -enable-experimental-concurrency \
// RUN:    | %FileCheck %s --check-prefix=CHECK
// REQUIRES: concurrency

// UNSUPPORTED: CPU=arm64e

func getString() async -> String {
  return ""
}

func wait() async throws {}

public func makeDinner() async throws -> String {
  let local_constant = 5
  let local = await getString()
  try await wait()
// CHECK-LABEL: define {{.*}} void @"$s1a10makeDinnerSSyYaKFTQ0_"
// CHECK-NEXT: entryresume.0:
// CHECK-NOT: {{ ret }}
// CHECK: call void @llvm.dbg.declare(metadata {{.*}}%0, metadata ![[LOCAL:[0-9]+]], {{.*}}!DIExpression(DW_OP_deref
// CHECK: ![[LOCAL]] = !DILocalVariable(name: "local"
  return local
}
