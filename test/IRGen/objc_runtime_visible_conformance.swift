// RUN: %target-swift-frontend -enable-objc-interop -I %S/../Inputs/custom-modules %s -emit-ir | %FileCheck %s

import ObjCRuntimeVisible

protocol MyProtocol {}
protocol YourProtocol {}

extension A : MyProtocol {}
extension A : YourProtocol {}

// CHECK-LABEL: @"$SSo1AC32objc_runtime_visible_conformance10MyProtocolACMc"
// CHECK-SAME:    @"$S32objc_runtime_visible_conformance10MyProtocolMp"
// CHECK-SAME:    [2 x i8]* [[STRING_A:@[0-9]+]]
// CHECK-SAME:    @"$SSo1AC32objc_runtime_visible_conformance10MyProtocolACWP"
//   DirectObjCClassName
// CHECK-SAME:    i32 16
// CHECK:       [[STRING_A]] = private unnamed_addr constant [2 x i8] c"A\00"
