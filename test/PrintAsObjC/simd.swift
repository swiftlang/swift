// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -emit-module-doc -o %t %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/simd.swiftmodule -parse -emit-objc-header-path %t/simd.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck %s < %t/simd.h
// RUN: %check-in-clang %t/simd.h
// RUN: %check-in-clang -fno-modules %t/simd.h -include Foundation.h

import Foundation
import SIMD

// CHECK-LABEL: typedef float swift_Float4 __attribute__((__ext_vector_type__(4)));
// CHECK-LABEL: typedef double swift_Double2 __attribute__((__ext_vector_type__(2)));
// CHECK-LABEL: typedef int swift_Int3 __attribute__((__ext_vector_type__(3)));

// -- There is no C SIMD module to import.
// CHECK-NOT: @import SIMD;

// CHECK-LABEL: @interface Foo : NSObject
@objc class Foo: NSObject {
  // CHECK-LABEL: - (swift_Float4)doStuffWithFloat4:(swift_Float4)x;
  @objc func doStuffWithFloat4(x: Float4) -> Float4 { return x }
  // CHECK-LABEL: - (swift_Double2)doStuffWithDouble2:(swift_Double2)x;
  @objc func doStuffWithDouble2(x: Double2) -> Double2 { return x }
  // CHECK-LABEL: - (swift_Int3)doStuffWithInt3:(swift_Int3)x;
  @objc func doStuffWithInt3(x: Int3) -> Int3 { return x }
}

