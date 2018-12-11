// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/weak.swiftmodule -module-name=weak %S/Inputs/weakly_linked.swift
// RUN: %target-swift-frontend -emit-ir %s -I %t | %FileCheck %s -DINT=i%target-ptrsize
import weak

// We should not hoist the metadata accessor accross the version check.

// CHECK-LABEL: define{{.*}} void @"$s24resilience_weakly_linked015test_not_hoist_b1_C0yyF"()
// CHECK-NOT: 15ResilientStructVMa
// CHECK: getVersion
// CHECK: 15ResilientStructVMa
// CHECK: ret

public func test_not_hoist_weakly_linked() {
  if getVersion() == 1 {
     var s = ResilientStruct()
     s.fn(5)
  }
}
