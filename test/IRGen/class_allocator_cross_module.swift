// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -emit-module -emit-module-path %t/Library.swiftmodule -module-name Library %S/../Interpreter/Inputs/class_allocator_fragile_library.swift -default-class-allocator Library.LibraryPool
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -emit-module -emit-module-path %t/ResilientLibrary.swiftmodule -module-name ResilientLibrary %S/../Interpreter/Inputs/class_allocator_resilient_library.swift -enable-library-evolution
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -I %t -emit-ir -O -module-name Client | %FileCheck %s

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: CPU=arm64 || CPU=x86_64

import Library
import ResilientLibrary

// Subclasses of a library built without library evolution inherit its
// classes' allocators, including the ones from its -default-class-allocator,
// which its module records. They call them directly.
// CHECK: @"$s6Client0A7DerivedCMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_7Library0A4PoolO, {{.*}}, i32 34,
public class ClientDerived: LibraryDerived {}

// CHECK: @"$s6Client0A9DefaultedCMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_7Library0A4PoolO, {{.*}}, i32 34,
public final class ClientDefaulted: LibraryDefaulted {}

// CHECK: @"$s6Client0A8OptedOutCMf" = internal global {{.*}} ptr @"$sBoWV", {{.*}}, i32 2,
public final class ClientOptedOut: LibraryOptedOut {}

public final class UnownedHolder {
  public unowned let fragile: LibraryFinal
  public unowned let resilient: ResilientShape
  public init(_ fragile: LibraryFinal, _ resilient: ResilientShape) {
    self.fragile = fragile
    self.resilient = resilient
  }
}

// CHECK-LABEL: define {{.*}}@"$s6Client0A7DerivedCfD"(
// CHECK-NOT: @swift_deallocClassInstance(
// CHECK: call swiftcc ptr @swift_deallocClassInstanceBegin(
// CHECK: call swiftcc void @"$s7Library0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(
// CHECK: }

// The client knows the allocator of the fragile library's final class. The
// resilient library's class may have subclasses with allocators, which the
// runtime frees.
// CHECK-LABEL: define {{.*}}@"$s6Client13UnownedHolderCfd"(
// CHECK: [[ALLOCATION:%.*]] = tail call ptr @swift_unownedReleaseBegin(
// CHECK: call swiftcc void @"$s7Library0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr nonnull [[ALLOCATION]],
// CHECK: call void @swift_unownedRelease(
// CHECK: }

// CHECK-LABEL: define {{.*}}@"$s6Client9makeLeaf1AA0A7DerivedCyF"(
// CHECK-NOT: @swift_allocObject(
// CHECK: call swiftcc ptr @"$s7Library0A4PoolO8allocate9byteCount9alignment3forSvSi_SiyXlXptFZ"(
// CHECK: call swiftcc ptr @swift_initHeapObject(
// CHECK: }
public func makeLeaf1() -> ClientDerived {
  return ClientDerived()
}

// An inlined initializer of the library's class calls its allocator.
// CHECK-LABEL: define {{.*}}@"$s6Client9makeFinal7Library0dC0CyF"(
// CHECK-NOT: @swift_allocObject(
// CHECK: call swiftcc ptr @"$s7Library0A4PoolO8allocate9byteCount9alignment3forSvSi_SiyXlXptFZ"(
// CHECK: }
public func makeFinal() -> LibraryFinal {
  return LibraryFinal(1)
}

// A final class whose superclass's layout is resilient: the last unowned
// release loads the instance's size and alignment from its metadata, like the
// class's deinit, because the class's layout without the superclass's fields
// is incomplete.
@allocator(LibraryPool)
public final class ClientResilientFinal: ResilientOpenBase {
  public var z = 4
}

public final class UnownedResilientFinalHolder {
  public unowned let target: ClientResilientFinal
  public init(_ target: ClientResilientFinal) { self.target = target }
}

// CHECK-LABEL: define {{.*}}@"$s6Client27UnownedResilientFinalHolderCfd"(
// CHECK: [[ALLOCATION:%.*]] = tail call ptr @swift_unownedReleaseBegin(
// CHECK: [[SIZE32:%.*]] = load i32
// CHECK: [[SIZE:%.*]] = zext i32 [[SIZE32]] to i64
// CHECK: call swiftcc void @"$s7Library0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr nonnull [[ALLOCATION]], i64 [[SIZE]],
// CHECK: }
