// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -enable-library-evolution -emit-module -target x86_64-apple-macosx10.60 -emit-module-path %t/weak_import_extension_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_extension_helper.swift
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -target x86_64-apple-macosx10.50 | %FileCheck %s --check-prefix=CHECK-OLD
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -target x86_64-apple-macosx10.60 | %FileCheck %s --check-prefix=CHECK-NEW
//
// REQUIRES: OS=macosx

import weak_import_extension_helper

@available(macOS 10.60, *)
public func callsExtensionMethod() {
  Foo().extensionMethod()
}

// CHECK-OLD: declare extern_weak swiftcc %swift.metadata_response @"$s28weak_import_extension_helper3FooVMa"
// CHECK-OLD: declare extern_weak swiftcc void @"$s28weak_import_extension_helper3FooVACycfC"
// CHECK-OLD: declare extern_weak swiftcc void @"$s28weak_import_extension_helper3FooV0C6MethodyyF"

// CHECK-NEW: declare swiftcc %swift.metadata_response @"$s28weak_import_extension_helper3FooVMa"
// CHECK-NEW: declare swiftcc void @"$s28weak_import_extension_helper3FooVACycfC"
// CHECK-NEW: declare swiftcc void @"$s28weak_import_extension_helper3FooV0C6MethodyyF"