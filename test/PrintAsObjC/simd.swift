// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/simd.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -emit-module-doc -o %t -module-name simd_test %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -parse-as-library %t/simd_test.swiftmodule -typecheck -emit-objc-header-path %t/simd.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/simd.h
// RUN: %check-in-clang %t/simd.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/simd.h

// REQUIRES: objc_interop

import Foundation
import simd

// CHECK-LABEL: typedef float swift_float4 __attribute__((__ext_vector_type__(4)));
// CHECK-LABEL: typedef double swift_double2 __attribute__((__ext_vector_type__(2)));
// CHECK-LABEL: typedef int swift_int3 __attribute__((__ext_vector_type__(3)));
// CHECK-LABEL: typedef unsigned int swift_uint4 __attribute__((__ext_vector_type__(4)));

// -- The C simd module is useless to Swift.
// CHECK-NOT: @import simd;

// CHECK-LABEL: @interface Foo : NSObject
@objc class Foo: NSObject {
  // CHECK-LABEL: - (swift_float4)doStuffWithFloat4:(swift_float4)x SWIFT_WARN_UNUSED_RESULT;
  @objc func doStuffWithFloat4(_ x: float4) -> float4 { return x }
  // CHECK-LABEL: - (swift_double2)doStuffWithDouble2:(swift_double2)x SWIFT_WARN_UNUSED_RESULT;
  @objc func doStuffWithDouble2(_ x: double2) -> double2 { return x }
  // CHECK-LABEL: - (swift_int3)doStuffWithInt3:(swift_int3)x SWIFT_WARN_UNUSED_RESULT;
  @objc func doStuffWithInt3(_ x: int3) -> int3 { return x }
  // CHECK-LABEL: - (swift_uint4)doStuffWithUInt4:(swift_uint4)x SWIFT_WARN_UNUSED_RESULT;
  @objc func doStuffWithUInt4(_ x: uint4) -> uint4 { return x }
}

