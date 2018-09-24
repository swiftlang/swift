// RUN: rm -rf %t
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -use-jit -playground -parse-stdlib %s -emit-ir -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Swift

@objc class C { }

public func anchor() {}

anchor()

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} i32 @main
// CHECK:         call void @runtime_registration
// CHECK:         call swiftcc void @"$s10playground6anchoryyF"
// CHECK:         ret void
// CHECK:       }

// CHECK-LABEL: define{{( protected)?}} private void @runtime_registration
// CHECK:         call void @swift_instantiateObjCClass({{.*}} @"$s10playground1CCN"

