// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -emit-api-descriptor-path %t.json
// RUN: %validate-json %t.json | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public struct MyStruct {
}

extension MyStruct: Equatable {
   public static func == (lhs: Self, rhs: Self) -> Bool { return false }
}

// CHECK:      "target"
// CHECK-NEXT: "globals": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "{{(_main)|(main)}}",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   }
// CHECK-NEXT: ],
// CHECK-NEXT: "interfaces": [],
// CHECK-NEXT: "categories": [],
// CHECK-NEXT: "version": "1.0"
