// RUN: rm -rf %t
// RUN: %swift -playground -parse-stdlib -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

import Swift

@objc class C { }

println()

// CHECK-LABEL: define internal void @top_level_code
// CHECK:         call void @swift_instantiateObjCClass({{.*}} @_TMdC10playground1C
// CHECK:         call void @_TFSs7printlnFT_T_
// CHECK:         ret void
// CHECK:       }
