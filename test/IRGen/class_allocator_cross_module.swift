// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -emit-module -emit-module-path %t/Library.swiftmodule -module-name Library %S/../Interpreter/Inputs/class_allocator_library.swift -D FRAGILE
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -I %t -emit-ir -O -module-name Client | %FileCheck %s

// REQUIRES: swift_feature_ClassAllocators

import Library

// A subclass in another module inherits the allocator recorded in the
// serialized module: its metadata is flagged, and it allocates and frees
// through the runtime, which finds the allocator from the metadata.
public class ClientDerived: LibraryDerived {}

// CHECK-LABEL: define {{.*}}@"$s6Client0A7DerivedCfD"(
// CHECK: call void @swift_deallocClassInstanceMaybeClassAllocated(
// CHECK: }

// CHECK-LABEL: define {{.*}}@"$s6Client9makeLeaf1AA0A7DerivedCyF"(
// CHECK-NOT: LibraryPool
// CHECK: call noalias ptr @swift_allocObjectMaybeClassAllocated(
// CHECK: }
public func makeLeaf1() -> ClientDerived {
  return ClientDerived()
}

// Inlined initializers of the library's classes do not call the library's
// allocator directly.
// CHECK-LABEL: define {{.*}}@"$s6Client9makeFinal7Library0dC0CyF"(
// CHECK-NOT: LibraryPool
// CHECK: call noalias ptr @swift_allocObjectMaybeClassAllocated(
// CHECK: }
public func makeFinal() -> LibraryFinal {
  return LibraryFinal(1)
}
