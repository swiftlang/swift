// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/Core.swiftmodule -module-name=Core %S/Inputs/original_defined_in_conformance_core.swift
// RUN: %target-swift-frontend -I %t -emit-ir -module-name main %s | %FileCheck %s
// REQUIRES: OS=macosx

import Core

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4main8callTakeyyF"()
public func callTake() {
  // CHECK: [[WTABLE:%.*]] = call ptr @"$s3Lib8MyStructVAC4Core0B8ProtocolAAWl"()
  // CHECK: call swiftcc void @"$s4Core4takeyyxAA10MyProtocolRzlF"(ptr noalias {{.*}}, ptr {{.*}}, ptr [[WTABLE]])
  // CHECK-NOT: @"$s3Lib8MyStructV4Core0B8ProtocolAAWP"
  take(MyStruct())
}
