// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -target %target-future-triple -module-name ResilientBases -emit-module-path %t/ResilientBases.swiftmodule %S/Inputs/class_allocator_resilient_bases.swift
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -primary-file %s -emit-ir -module-name M | %FileCheck %s

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: CPU=arm64 || CPU=x86_64

// Metadata that the runtime builds or completes gets the class's value
// witness table without new runtime entry points.

import ResilientBases

public enum Pool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

// A generic class's instantiation function installs the table.
@allocator(Pool)
public final class GenericBox<T> {
  public var value: T
  public init(_ value: T) { self.value = value }
}

// A class with resilient ancestry has a relocation function that installs it.
// CHECK: @"$s1M12ResilientSubCMP" = internal constant {{.*}} @"__swift_relocate_class_metadata_$s1M12ResilientSubCMn"
@allocator(Pool)
public final class ResilientSub: ResilientBase {}

// CHECK: @"$s1M19ResilientGenericSubCMP" = internal constant {{.*}} @"__swift_relocate_class_metadata_$s1M19ResilientGenericSubCMn"
@allocator(Pool)
public final class ResilientGenericSub: ResilientGenericBase<Int> {}

// A class without an allocator uses the default relocation.
// CHECK: @"$s1M17PlainResilientSubCMP" = internal constant <{ i32, i32, i32, i32, i32, i32 }> <{ i32 0,
public final class PlainResilientSub: ResilientBase {}

// Static templates of singleton-initialized metadata contain the table, which
// the runtime keeps.
// CHECK: @"$s1M18WithResilientFieldCMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_1M4PoolO,
@allocator(Pool)
public final class WithResilientField {
  public var field = ResilientStruct()
  public init() {}
}

public class LocalGenericBase<T> {
  public var t: T?
  public init() {}
}

// CHECK: @"$s1M15GenericAncestryCMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_1M4PoolO,
@allocator(Pool)
public final class GenericAncestry: LocalGenericBase<Int> {}

// CHECK-LABEL: define internal ptr @"$s1M10GenericBoxCMi"(ptr %0, ptr %1, ptr %2)
// CHECK: [[METADATA:%.*]] = call ptr @swift_allocateGenericClassMetadata(ptr %0, ptr %1, ptr %2)
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[METADATA]], i32 -1
// CHECK: store ptr @__swift_class_allocator_value_witnesses_1M4PoolO, ptr [[SLOT]]
// CHECK: ret ptr [[METADATA]]


// CHECK-LABEL: define linkonce_odr hidden ptr @"__swift_relocate_class_metadata_$s1M12ResilientSubCMn"(ptr %0, ptr %1)
// CHECK: [[METADATA:%.*]] = call ptr @swift_relocateClassMetadata(ptr %0, ptr %1)
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[METADATA]], i32 -1
// CHECK: store ptr @__swift_class_allocator_value_witnesses_1M4PoolO, ptr [[SLOT]]
// CHECK: ret ptr [[METADATA]]

// CHECK-LABEL: define internal ptr @"$s1M16LocalGenericBaseCMi"(
// CHECK-NOT: value_witnesses
// CHECK: ret ptr
