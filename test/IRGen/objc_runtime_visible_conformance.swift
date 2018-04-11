// REQUIRES: objc_interop

// RUN: %target-swift-frontend -I %S/../Inputs/custom-modules %s -emit-ir | %FileCheck %s

import ObjCRuntimeVisible

protocol MyProtocol {}

extension A : MyProtocol {}

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$SSo1ACMa"({{i32|i64}}) {{.*}} {
// CHECK: call %objc_class* @objc_lookUpClass
// CHECK: call %swift.type* @swift_getObjCClassMetadata
// CHECK: ret
