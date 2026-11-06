// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-experimental-feature BackDeployFrozenValueTypes -enable-library-evolution -module-name MyKit %S/Inputs/back_deployed_type_transparent_module.swift -o %t/MyKit.swiftmodule
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name Client %s -I %t > %t/client.ll
// RUN: %FileCheck %s < %t/client.ll

// REQUIRES: swift_feature_BackDeployFrozenValueTypes
// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

// A protocol witness thunk isn't mandatory-inlined into, so it refers to a
// transparent member of a back-deployed type. The member might not exist in
// the OS, so the client must have its own copy rather than a weak reference
// to the original.

import MyKit

public func useWide() -> Int {
  Wide.size
}

// CHECK-NOT: extern_weak{{.*}}$s5MyKit4WideV4sizeSivgZ
// CHECK: define linkonce_odr hidden swiftcc i64 @"$s5MyKit4WideV4sizeSivgZ"
