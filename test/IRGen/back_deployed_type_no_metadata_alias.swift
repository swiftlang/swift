// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-experimental-feature BackDeployFrozenValueTypes -enable-library-evolution -module-name MyKit %S/Inputs/back_deployed_type_module.swift -o %t/MyKit.swiftmodule
// RUN: %target-swift-frontend -emit-ir -parse-as-library -I %t %s | %FileCheck %s

// REQUIRES: swift_feature_BackDeployFrozenValueTypes
// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

// The copy of a non-generic back-deployed type's metadata that a client emits
// has shared linkage. An alias to the address point can't be shared on Mach-O
// (aliases aren't weak definitions), so linking two objects that both use the
// type failed with a duplicate symbol. Refer into the full metadata instead.

import MyKit

public func use() -> Any.Type { Point.self }

// CHECK-NOT: alias {{.*}}5MyKit5PointVN
// CHECK: @"$s5MyKit5PointVMf" = linkonce_odr hidden
