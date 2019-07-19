// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-interface-path %t/Lib.swiftinterface -typecheck -enable-library-evolution -enable-objc-interop -disable-objc-attr-requires-foundation-module -swift-version 5 %S/Inputs/enums-layout-helper.swift -module-name Lib
// RUN: %FileCheck %S/Inputs/enums-layout-helper.swift < %t/Lib.swiftinterface
// RUN: %target-swift-frontend -enable-objc-interop -O -emit-ir -primary-file %s -I %t | %FileCheck %s

import Lib

// CHECK-LABEL: define{{.+}}testFutureproofEnum
func testFutureproofEnum() -> FutureproofEnum {
  // Check a few things in the function to make sure it's getting the case
  // representation dynamically.
  // CHECK: [[CASE:%.+]] = load i32, i32* @"$s3Lib15FutureproofEnumO1byA2CmFWC"
  // CHECK: [[METADATA_RESPONSE:%.+]] = tail call swiftcc %swift.metadata_response @"$s3Lib15FutureproofEnumOMa"
  // CHECK: [[METADATA:%.+]] = extractvalue %swift.metadata_response [[METADATA_RESPONSE]], 0
  // CHECK: call void {{%.+}}(%swift.opaque* noalias %0, i32 [[CASE]], %swift.type* [[METADATA]])
  // CHECK-NEXT: ret void
  return .b
} // CHECK-NEXT: {{^}$}}

// CHECK-LABEL: define{{.+}}testFrozenEnum
func testFrozenEnum() -> FrozenEnum {
  // CHECK: ret i8 1
  return .b
} // CHECK-NEXT: {{^}$}}

// CHECK-LABEL: define{{.+}}testFutureproofObjCEnum
func testFutureproofObjCEnum() -> FutureproofObjCEnum {
  // CHECK: ret i{{32|64}} 10
  return .b
} // CHECK-NEXT: {{^}$}}

// CHECK-LABEL: define{{.+}}testFrozenObjCEnum
func testFrozenObjCEnum() -> FrozenObjCEnum {
  // CHECK: ret i{{32|64}} 10
  return .b
} // CHECK-NEXT: {{^}$}}
