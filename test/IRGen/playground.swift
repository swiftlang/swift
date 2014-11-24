// RUN: rm -rf %t
// RUN: %swift -use-jit -playground -parse-stdlib -target x86_64-apple-macosx10.9 %s -emit-ir | FileCheck %s
// XFAIL: linux

import Swift

@objc class C { }

println()

// CHECK-LABEL: define i32 @main
// CHECK:         call void @runtime_registration
// CHECK:         call void @_TFSs7printlnFT_T_
// CHECK:         ret void
// CHECK:       }

// CHECK-LABEL: define private void @runtime_registration
// CHECK:         call void @swift_instantiateObjCClass({{.*}} @_TMdC10playground1C

