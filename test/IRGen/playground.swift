// RUN: rm -rf %t
// RUN: %target-swift-frontend -use-jit -playground -parse-stdlib %s -emit-ir -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Swift

@objc class C { }

public func anchor() {}

anchor()

// CHECK-LABEL: define i32 @main
// CHECK:         call void @runtime_registration
// CHECK:         call void @_TF10playground6anchorFT_T_
// CHECK:         ret void
// CHECK:       }

// CHECK-LABEL: define private void @runtime_registration
// CHECK:         call void @swift_instantiateObjCClass({{.*}} @_TMdC10playground1C

