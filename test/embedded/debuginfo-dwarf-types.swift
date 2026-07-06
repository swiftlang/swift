// RUN: %target-swift-frontend -g -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-G
// RUN: %target-swift-frontend -g -O -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-G
// RUN: %target-swift-frontend -g -Osize -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-G
// RUN: %target-swift-frontend -gdwarf-types -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-G
// RUN: %target-swift-frontend -gdwarf-types -O -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-G
// RUN: %target-swift-frontend -gdwarf-types -Osize -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-G
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-GNONE
// RUN: %target-swift-frontend -O -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-GNONE
// RUN: %target-swift-frontend -Osize -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=CHECK-GNONE

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public struct MyType {
    var x, y: Int
}
public func foo(_ t: MyType) {}

// CHECK-G: !DICompositeType(tag: DW_TAG_structure_type, name: "MyType"
// CHECK-G: !DIDerivedType(tag: DW_TAG_member, name: "x"
// CHECK-G: !DIDerivedType(tag: DW_TAG_member, name: "y"

// CHECK-GNONE-NOT: !DICompositeType(tag: DW_TAG_structure_type, name: "MyType"
