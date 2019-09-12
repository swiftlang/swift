// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-interface-path %t/Lib.swiftinterface -emit-module -o %t/unused.swiftmodule -enable-library-evolution -Xfrontend -enable-objc-interop -Xfrontend -disable-objc-attr-requires-foundation-module -swift-version 5 %S/Inputs/enums-layout-helper.swift -module-name Lib
// RUN: %FileCheck -check-prefix CHECK -check-prefix CHECK-MULTI-FILE %S/Inputs/enums-layout-helper.swift < %t/Lib.swiftinterface
// RUN: %target-swift-frontend -enable-objc-interop -O -emit-ir -primary-file %s -I %t -Xllvm -swiftmergefunc-threshold=0 | %FileCheck %s

// Try again using a single-frontend build.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -force-single-frontend-invocation -emit-module-interface-path %t/Lib.swiftinterface -emit-module -o %t/unused.swiftmodule -enable-library-evolution -Xfrontend -enable-objc-interop -Xfrontend -disable-objc-attr-requires-foundation-module -swift-version 5 %S/Inputs/enums-layout-helper.swift -module-name Lib
// RUN: %FileCheck -check-prefix CHECK -check-prefix CHECK-SINGLE-FRONTEND %S/Inputs/enums-layout-helper.swift < %t/Lib.swiftinterface
// RUN: %target-swift-frontend -enable-objc-interop -O -emit-ir -primary-file %s -I %t -Xllvm -swiftmergefunc-threshold=0 | %FileCheck %s


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

// CHECK-LABEL: define{{.+}}testFutureproofIndirectEnum
func testFutureproofIndirectEnum() -> FutureproofIndirectEnum {
  // CHECK: [[CASE:%.+]] = load i32, i32* @"$s3Lib23FutureproofIndirectEnumO1cyA2CmFWC"
  // CHECK: [[METADATA_RESPONSE:%.+]] = tail call swiftcc %swift.metadata_response @"$s3Lib23FutureproofIndirectEnumOMa"
  // CHECK: [[METADATA:%.+]] = extractvalue %swift.metadata_response [[METADATA_RESPONSE]], 0
  // CHECK: call void {{%.+}}(%swift.opaque* noalias %0, i32 [[CASE]], %swift.type* [[METADATA]])
  // CHECK-NEXT: ret void
  return .c
}

// CHECK-LABEL: define{{.+}}testFrozenIndirectEnum
func testFrozenIndirectEnum() -> FrozenIndirectEnum {
  // Whether this is "1" or "2" depends on whether the reserved ObjC tagged
  // pointer bit is the top or bottom bit on this platform.
  // CHECK: ret i{{32|64}} {{1|2}}
  return .c
}

// CHECK-LABEL: define{{.+}}testFutureproofIndirectCaseEnum
func testFutureproofIndirectCaseEnum() -> FutureproofIndirectCaseEnum {
  // CHECK: [[CASE:%.+]] = load i32, i32* @"$s3Lib27FutureproofIndirectCaseEnumO1cyA2CmFWC"
  // CHECK: [[METADATA_RESPONSE:%.+]] = tail call swiftcc %swift.metadata_response @"$s3Lib27FutureproofIndirectCaseEnumOMa"
  // CHECK: [[METADATA:%.+]] = extractvalue %swift.metadata_response [[METADATA_RESPONSE]], 0
  // CHECK: call void {{%.+}}(%swift.opaque* noalias %0, i32 [[CASE]], %swift.type* [[METADATA]])
  // CHECK-NEXT: ret void
  return .c
}

// CHECK-LABEL: define{{.+}}testFrozenIndirectCaseEnum
func testFrozenIndirectCaseEnum() -> FrozenIndirectCaseEnum {
  // Whether this is "1" or "2" depends on whether the reserved ObjC tagged
  // pointer bit is the top or bottom bit on this platform.
  // CHECK: ret i{{32|64}} {{1|2}}
  return .c
}
