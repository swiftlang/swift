// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-collocate-metadata-functions | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: define{{.*}} swiftcc %swift.metadata_response @"$s28collocate_metadata_functions13GenericStructVMr"({{.*}} section "__TEXT, __textg_swiftm, regular, pure_instructions"

public struct GenericStruct<T> {
    var field: T?
}
