// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

//import Foundation

class NormalEverydayClass {}
// CHECK: @"$s22objc_runtime_name_attr19NormalEverydayClassCMm" = hidden global %objc_class
// CHECK: @_DATA__TtC22objc_runtime_name_attr19NormalEverydayClass = internal constant


@_objcRuntimeName(RenamedClass)
class ThisWillBeRenamed {}
// CHECK: @"$s22objc_runtime_name_attr17ThisWillBeRenamedCMm" = hidden global %objc_class
// CHECK: @_DATA_RenamedClass = internal constant

