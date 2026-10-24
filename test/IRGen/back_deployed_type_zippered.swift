// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-library-evolution -module-name MyKitZip %S/Inputs/back_deployed_type_zippered_module.swift -o %t/MyKitZip.swiftmodule -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name Client %s -I %t -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi > %t/client.ll
// RUN: %FileCheck %s < %t/client.ll
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t/client.ll
// RUN: %FileCheck %s --check-prefix=MACONLY < %t/client.ll
// RUN: %FileCheck %s --check-prefix=BOTH < %t/client.ll

// REQUIRES: OS=macosx || OS=maccatalyst

// A zippered client emits its own copy of a back-deployed type unless both of
// its deployment targets guarantee that the type's module provides it.

import MyKitZip

// CHECK-DAG: @"__swift_back_deployment_anchor_$s8MyKitZip11VariantOnlyVMn" = private global
// CHECK-DAG: @"__swift_back_deployment_anchor_$s8MyKitZip11PrimaryOnlyVMn" = private global
// CHECK-DAG: @"__swift_back_deployment_anchor_$s8MyKitZip16NeededForVariantVMn" = private global
// NEGATIVE-NOT: __swift_back_deployment_anchor_$s8MyKitZip9NotNeededVMn

public func useVariantOnly() -> Any.Type { VariantOnly.self }
public func usePrimaryOnly() -> Any.Type { PrimaryOnly.self }
public func useNeededForVariant() -> Any.Type { NeededForVariant.self }
public func useNotNeeded() -> Any.Type { NotNeeded.self }

// A member that is only opted out of back deployment on macOS is still
// back-deployed for the target variant.
// MACONLY-LABEL: define {{.*}}@"$s6Client11callMacOnlyySi8MyKitZip4BothVF"(
// MACONLY: call swiftcc i64 @"$s8MyKitZip4BothV7macOnlySiyFTwb"(
@available(macOS 99, iOS 99, *)
public func callMacOnly(_ b: Both) -> Int { b.macOnly() }

// A member opted out on both platforms is called directly.
// BOTH-LABEL: define {{.*}}@"$s6Client8callBothySi8MyKitZip0C0VF"(
// BOTH: call swiftcc i64 @"$s8MyKitZip4BothV4bothSiyF"(
@available(macOS 99, iOS 99, *)
public func callBoth(_ b: Both) -> Int { b.both() }
