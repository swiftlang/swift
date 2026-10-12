// RUN: %target-swift-frontend -enable-experimental-feature Embedded -module-name test -parse-as-library %s -emit-ir | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

// check lines do not match ptrauch code
// UNSUPPORTED: CPU=arm64e

import _Concurrency

public var e: (any TaskExecutor)? = nil

// CHECK-LABEL: define {{.*}}@"$e4test6testits19UnownedTaskExecutorVyF"()
// CHECK: [[EXISTENTIAL_ADDR:%.*]] = call {{.*}}"$e4test1eSch_pSgvau"
// CHECK: [[INSTANCE_ADDR:%.*]] = getelementptr {{.*}}[[EXISTENTIAL_ADDR]]{{, i[0-9]+ 0, i[0-9]+ 0}}
// CHECK: [[INSTANCE:%.*]] = load ptr, ptr [[INSTANCE_ADDR]]
// CHECK: [[CONFORMANCE_ADDR:%.*]] = getelementptr {{.*}}[[EXISTENTIAL_ADDR]]{{, i[0-9]+ 0, i[0-9]+ 1}}
// CHECK: [[CONFORMANCE:%.*]] = load ptr, ptr [[CONFORMANCE_ADDR]]
// CHECK: icmp eq ptr [[INSTANCE]], null
// CHECK: [[INSTANCE_INT:%.*]] = ptrtoint ptr [[INSTANCE]] to i{{32|64}}
// CHECK: [[INSTANCE_PAYLOAD:%.*]] = inttoptr i{{32|64}} [[INSTANCE_INT]] to ptr
// CHECK: [[CONFORMANCE_INT:%.*]] = ptrtoint ptr [[CONFORMANCE]] to i{{32|64}}
// CHECK: [[CONFORMANCE_PAYLOAD:%.*]] = inttoptr i{{32|64}} [[CONFORMANCE_INT]] to ptr
// CHECK: [[INSTANCE_ISA:%.*]] = load ptr, ptr [[INSTANCE_PAYLOAD]]
// CHECK: call {{.*}}@"$es19UnownedTaskExecutorVyABxhcSchRzlufC"(ptr [[INSTANCE_PAYLOAD]], ptr [[INSTANCE_ISA]], ptr [[CONFORMANCE_PAYLOAD]])
// CHECK-LABEL: {{^}}}
public func testit() -> UnownedTaskExecutor {
  return unsafe UnownedTaskExecutor(e!)
}

