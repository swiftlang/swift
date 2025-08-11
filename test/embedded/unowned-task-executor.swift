// RUN: %target-swift-frontend -enable-experimental-feature Embedded -disable-availability-checking -module-name test -parse-as-library %s -emit-ir | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// check lines do not match ptrauch code
// UNSUPPORTED: CPU=arm64e

import _Concurrency

public var e: (any TaskExecutor)? = nil

// CHECK-LABEL: define {{.*}}@"$e4test6testits19UnownedTaskExecutorVyF"()
// CHECK: [[EXISTENTIAL_ADDR:%.*]] = call {{.*}}"$e4test1eSch_pSgvau"
// CHECK: [[INSTANCE_ADDR:%.*]] = getelementptr {{.*}}[[EXISTENTIAL_ADDR]]{{, i[0-9]+ 0, i[0-9]+ 0}}
// CHECK: [[INSTANCE:%.*]] = load {{.*}}ptr [[INSTANCE_ADDR]]
// CHECK: [[CONFORMANCE_ADDR:%.*]] = getelementptr {{.*}}[[EXISTENTIAL_ADDR]]{{, i[0-9]+ 0, i[0-9]+ 1}}
// CHECK: [[CONFORMANCE:%.*]] = load {{.*}}ptr [[CONFORMANCE_ADDR]]
// CHECK: {{^[0-9a-z]+:}}
// CHECK: [[INSTANCE_PTR:%.*]] = inttoptr {{i[0-9]+}} [[INSTANCE]]
// CHECK: [[CONFORMANCE_PTR:%.*]] = inttoptr {{i[0-9]+}} [[CONFORMANCE]]
// CHECK: [[INSTANCE_PHI:%.*]] = phi ptr [ [[INSTANCE_PTR]]
// CHECK: [[CONFORMANCE_PHI:%.*]] = phi ptr [ [[CONFORMANCE_PTR]]
// CHECK: [[INSTANCE_ISA:%.*]] = load ptr, ptr [[INSTANCE_PHI]]
// CHECK: call {{.*}}@"$es19UnownedTaskExecutorVyABxhcSchRzlufC"(ptr [[INSTANCE_PHI]], ptr [[INSTANCE_ISA]], ptr [[CONFORMANCE_PHI]])
// CHECK-LABEL: {{^}}}
public func testit() -> UnownedTaskExecutor {
  return unsafe UnownedTaskExecutor(e!)
}

