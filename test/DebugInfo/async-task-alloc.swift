// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name a  -target %target-swift-5.1-abi-triple \
// RUN:    | %FileCheck %s --check-prefix=CHECK
// REQUIRES: concurrency

// Test dynamically allocated local variables in async functions.

// CHECK-LABEL: define {{.*}} void @"$s1a1fyxxYalF"
// CHECK: swift_task_alloc
// CHECK-LABEL: define {{.*}} void @"$s1a1fyxxYalFTY0_"
// CHECK-NEXT: entryresume.0:
// CHECK-NEXT: #dbg_declare
// CHECK-NEXT: #dbg_declare({{.*}}%0, ![[DYNA:[0-9]+]], {{.*}}!DIExpression({{.*}}DW_OP_deref
// CHECK-NEXT: #dbg_declare({{.*}}%0, ![[T:[0-9]+]], {{.*}}!DIExpression({{.*}}DW_OP_deref

// CHECK: ![[DYNA]] = !DILocalVariable(name: "dyna"
// CHECK: ![[T]] = !DILocalVariable(name: "t"
public func f<T>(_ t: T) async -> T {
  let dyna = t
  return dyna
}
