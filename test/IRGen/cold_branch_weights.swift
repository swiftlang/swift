// Test the lowering of ColdBlockInfo into LLVM IR via !prof branch weights and `cold` LLVM IR functions.
// Static branch prediction is used here to seed the ColdBlockInfo with data.

// RUN: %target-swift-frontend %s -O \
// RUN:   -enable-throws-prediction \
// RUN:   -module-name=test -emit-irgen \
// RUN:       | %FileCheck --check-prefix CHECK-ENABLED %s

// RUN: %target-swift-frontend %s -O \
// RUN:   -module-name=test -emit-irgen \
// RUN:       | %FileCheck --check-prefix CHECK-DISABLED %s

// At -Onone, don't expect the lowering to happen.
// RUN: %target-swift-frontend %s -Onone \
// RUN:   -enable-throws-prediction \
// RUN:   -module-name=test -emit-irgen \
// RUN:       | %FileCheck --check-prefix CHECK-DISABLED %s

enum MyError: Error {
  case err
}

// CHECK-ENABLED-LABEL: define {{.*}}@"${{.*}}condThrows{{.*}}
// CHECK-ENABLED:   br i1 %0, {{.*}} !prof [[WEIGHTS:![0-9]+]]

// CHECK-DISABLED-LABEL: define {{.*}}@"${{.*}}condThrows{{.*}}
// CHECK-DISABLED-NOT:   !prof
public func condThrows(_ b: Bool) throws -> Int {
  if b {
    return 42
  }
  throw MyError.err
}

// CHECK-ENABLED: define {{.*}}@"${{.*}}alwaysThrows{{.*}} #[[ATTRS:[0-9]+]]
// CHECK-DISABLED: define {{.*}}@"${{.*}}alwaysThrows{{.*}} #[[ATTRS:[0-9]+]] {
public func alwaysThrows(_ b: Bool) throws {
  if b {
    throw MyError.err
  } else {
    throw MyError.err
  }
}

// CHECK-ENABLED-DAG: [[WEIGHTS]] = !{!"branch_weights", i32 2001, i32 2}
// CHECK-ENABLED-DAG: attributes #[[ATTRS]] = {{{.*}}cold{{.*}}}

// CHECK-DISABLED-NOT: attributes #[[ATTRS]] = {{{.*}}cold{{.*}}}
