// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name a  -disable-availability-checking \
// RUN:    | %FileCheck %s --check-prefix=CHECK
// REQUIRES: concurrency
// REQUIRES: CPU=x86_64 || CPU=arm64

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
// CHECK: call void @llvm.dbg.declare(metadata {{.*}}%0, metadata ![[LOCAL:[0-9]+]], {{.*}}!DIExpression(DW_OP_LLVM_entry_value, 1, DW_OP_deref, DW_OP_plus_uconst, {{[0-9]+}})
// CHECK: ![[LOCAL]] = !DILocalVariable(name: "local"
  return local
}
