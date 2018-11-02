// RUN: %target-swift-frontend -enable-objc-interop -I %S/../Inputs/custom-modules %s -emit-ir | %FileCheck %s

import ObjCRuntimeVisible

protocol MyProtocol {}
protocol YourProtocol {}

extension A : MyProtocol {}
extension A : YourProtocol {}

// CHECK-LABEL: @"$SSo1ACMn" = linkonce_odr hidden constant <{ {{.*}} }>

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$SSo1ACMa"({{i32|i64}}) {{.*}} {
// CHECK: call %objc_class* @objc_lookUpClass
// CHECK: call %swift.type* @swift_getObjCClassMetadata
// CHECK: ret
