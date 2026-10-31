// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -emit-ir -module-name M | %FileCheck %s

// REQUIRES: swift_feature_ClassAllocators

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

@allocator(Pool)
public class Base {
  public init() {}
}

public class Derived: Base {}

@allocator(Pool)
public final class GenericBox<T> {
  public var value: T
  public init(_ value: T) { self.value = value }
}

public final class Plain {
  public init() {}
}

// The descriptor of a class that declares an allocator has the "has class
// allocator" flag (0x100) and ends with a reference to the allocator access
// function. Subclasses that inherit it have neither.
// CHECK: @"$s1M4BaseCMn" = constant <{ {{.*}} }> <{ i32 -2147483312, {{.*}}@"__swift_class_allocator_$s1M4BaseCMn"
// The metadata of every class with an allocator, declared or inherited, has
// the ClassFlags::HasCustomAllocator flag (0x20).
// CHECK: @"$s1M4BaseCMf" = {{.*}}, i32 34, i32 0, i32 16, i16 7,
// CHECK: @"$s1M7DerivedCMn" = constant <{ {{.*}} }> <{ i32 1073741904,
// CHECK-NOT: __swift_class_allocator_$s1M7DerivedCMn
// CHECK: @"$s1M7DerivedCMf" = {{.*}}, i32 34, i32 0, i32 16, i16 7,
// CHECK: @"$s1M10GenericBoxCMn" = {{.*}}@"__swift_class_allocator_$s1M10GenericBoxCMn"
// CHECK: @"$s1M5PlainCMn" = constant <{ {{.*}} }> <{ i32 -2147483568,
// CHECK: @"$s1M5PlainCMf" = {{.*}}, i32 2, i32 0, i32 16, i16 7,

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s1M4BaseCACycfC"(
// CHECK: call noalias ptr @swift_allocObjectMaybeClassAllocated(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc void @"$s1M4BaseCfD"(
// CHECK: call void @swift_deallocClassInstanceMaybeClassAllocated(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc void @"$s1M7DerivedCfD"(
// CHECK: call void @swift_deallocClassInstanceMaybeClassAllocated(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s1M10GenericBoxCyACyxGxcfC"(
// CHECK: call noalias ptr @swift_allocObjectMaybeClassAllocated(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s1M5PlainCACycfC"(
// CHECK: call noalias ptr @swift_allocObject(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc void @"$s1M5PlainCfD"(
// CHECK: call void @swift_deallocClassInstance(
// CHECK: }

// The access function returns the allocator's metadata and the witness table
// of its conformance.
// CHECK-LABEL: define linkonce_odr hidden swiftcc { ptr, ptr } @"__swift_class_allocator_$s1M4BaseCMn"()
// CHECK: [[WITNESS:%.*]] = call ptr @"$s1M4PoolOACs14ClassAllocatorAAWl"()
// CHECK: [[RESULT:%.*]] = insertvalue { ptr, ptr } { ptr getelementptr inbounds ({{.*}}, ptr @"$s1M4PoolOMf", i32 0, i32 2), ptr undef }, ptr [[WITNESS]], 1
// CHECK: ret { ptr, ptr } [[RESULT]]
