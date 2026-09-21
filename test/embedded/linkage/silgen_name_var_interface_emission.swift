// REQUIRES: swift_feature_Embedded

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -emit-module -o %t/Library.ll %s -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -parse-as-library -module-name Library
// RUN: %FileCheck %s < %t/Library.ll

// Under `CodeGenerationModel=interface` the library emits the storage as
// a strong definition so clients can reference it externally.
// CHECK-DAG: @my_renamed_storage = {{(dso_local |protected |linkonce_odr |weak )*}}global
// CHECK-NOT: @my_renamed_storage = available_externally
// CHECK-DAG: define {{(hidden |protected |dllexport )?}}swiftcc ptr @"$e7Library14renamedStorageSVSg_ACtvau"
@_silgen_name("my_renamed_storage")
@export(interface)
var renamedStorage: (UnsafeRawPointer?, UnsafeRawPointer?) = (nil, nil)

// Force the accessor (and therefore a reference to the storage) to be
// kept after dead-code elimination.
public func touch() -> UnsafeRawPointer? {
  return renamedStorage.0
}

