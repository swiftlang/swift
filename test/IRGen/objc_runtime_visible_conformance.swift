// RUN: %target-swift-frontend -enable-objc-interop -I %S/../Inputs/custom-modules %s -emit-ir | %FileCheck %s
// REQUIRES: objc_codegen

import ObjCRuntimeVisible

protocol MyProtocol {}
protocol YourProtocol {}

extension A : MyProtocol {}
extension A : YourProtocol {}

// CHECK-LABEL: @"$sSo1AC32objc_runtime_visible_conformance10MyProtocolACMc"
// CHECK-SAME:    @"$s32objc_runtime_visible_conformance10MyProtocolMp"
// CHECK-SAME:    @.str.21.MyRuntimeVisibleClass
// CHECK-SAME:    @"$sSo1AC32objc_runtime_visible_conformance10MyProtocolACWP"
//   DirectObjCClassName
// CHECK-SAME:    i32 16
// CHECK:       @.str.21.MyRuntimeVisibleClass = private constant [22 x i8] c"MyRuntimeVisibleClass\00"
