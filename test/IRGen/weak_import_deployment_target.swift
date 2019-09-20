// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -enable-library-evolution -emit-module -target x86_64-apple-macosx10.50 -emit-module-path %t/weak_import_deployment_target_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_deployment_target_helper.swift
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -target x86_64-apple-macosx10.50 | %FileCheck %s --check-prefix=CHECK-OLD
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -target x86_64-apple-macosx10.60 | %FileCheck %s --check-prefix=CHECK-NEW
//
// RUN: %target-swift-frontend -enable-library-evolution -emit-module -target x86_64-apple-macosx10.60 -emit-module-path %t/weak_import_deployment_target_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_deployment_target_helper.swift
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -target x86_64-apple-macosx10.50 | %FileCheck %s --check-prefix=CHECK-OLD
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -target x86_64-apple-macosx10.60 | %FileCheck %s --check-prefix=CHECK-NEW
//
// REQUIRES: OS=macosx

import weak_import_deployment_target_helper

@available(macOS 10.50, *)
public func callsOld() {
  hasDefaultArg()
}

@available(macOS 10.60, *)
public func callsNew() {
  hasAvailableDefaultArg()
}

// CHECK-OLD-LABEL: declare swiftcc void @"$s36weak_import_deployment_target_helper13hasDefaultArgyySiF"(i64)
// CHECK-OLD-LABEL: declare extern_weak swiftcc void @"$s36weak_import_deployment_target_helper22hasAvailableDefaultArgyySiF"(i64)
// CHECK-OLD-LABEL: declare extern_weak swiftcc i64 @"$s36weak_import_deployment_target_helper17availableFunctionSiyF"()
// CHECK-OLD-LABEL: declare swiftcc i64 @"$s36weak_import_deployment_target_helper8functionSiyF"()

// CHECK-NEW-LABEL: declare swiftcc void @"$s36weak_import_deployment_target_helper13hasDefaultArgyySiF"(i64)
// CHECK-NEW-LABEL: declare swiftcc void @"$s36weak_import_deployment_target_helper22hasAvailableDefaultArgyySiF"(i64)
// CHECK-NEW-LABEL: declare swiftcc i64 @"$s36weak_import_deployment_target_helper17availableFunctionSiyF"()
// CHECK-NEW-LABEL: declare swiftcc i64 @"$s36weak_import_deployment_target_helper8functionSiyF"()