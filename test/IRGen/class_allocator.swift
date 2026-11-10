// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -emit-ir -module-name M -Onone | %FileCheck %s --check-prefixes=CHECK,ONONE
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -emit-ir -module-name M -O -disable-llvm-merge-functions-pass | %FileCheck %s --check-prefixes=CHECK,OPT

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: CPU=arm64 || CPU=x86_64

@inline(never)
public func poolAllocate(_ byteCount: Int, _ alignment: Int) -> UnsafeMutableRawPointer {
  UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
}

@inline(never)
public func poolDeallocate(_ pointer: UnsafeMutableRawPointer) {
  unsafe pointer.deallocate()
}

public enum Pool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

@allocator(Pool)
public class Base {
  public init() {}
}

public class Derived: Base {}

@allocator(SystemClassAllocator)
public final class OptedOut: Base {}

@allocator(Pool)
public final class GenericBox<T> {
  public var value: T
  public init(_ value: T) { self.value = value }
}

public final class Plain {
  public init() {}
}

@allocator(Pool)
public final class Leaf {
  public init() {}
}

public final class UnownedHolder {
  public unowned let leaf: Leaf
  public unowned let base: Base
  public init(_ leaf: Leaf, _ base: Base) {
    self.leaf = leaf
    self.base = base
  }
}

@allocator(Pool)
public final class FixedGenericBox<T> {
  public var count: Int
  public init() { count = 0 }
}

public final class GenericUnownedHolder {
  public unowned let fixed: FixedGenericBox<String>
  public unowned let box: GenericBox<Int>
  public init(_ fixed: FixedGenericBox<String>, _ box: GenericBox<Int>) {
    self.fixed = fixed
    self.box = box
  }
}

// Allocators whose `deallocate` can't be the class allocator witness.
public final class ClassAllocatorBox: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

public struct GenericPool<T>: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

public protocol DefaultingPool {}

@available(SwiftStdlib 6.5, *)
extension ClassAllocator where Self: DefaultingPool {
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

public enum DefaultPool: @unsafe ClassAllocator, DefaultingPool {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
}

@allocator(ClassAllocatorBox)
public final class ClassAllocated {}

@allocator(GenericPool<Int>)
public final class GenericAllocated {}

@allocator(DefaultPool)
public final class DefaultAllocated {}

// Classes with the same allocator share one value witness table:
// Builtin.NativeObject's witnesses and flags, then the class allocator witness.
// No value witness flag marks it. The witness is the enum's own `deallocate`,
// whose lowering is the witness's.
// CHECK: @__swift_class_allocator_value_witnesses_1M4PoolO = linkonce_odr hidden constant { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64, i32, i32, ptr } { ptr @__swift_initWithCopy_strong, ptr @__swift_destroy_strong, ptr @__swift_initWithCopy_strong, ptr @__swift_assignWithCopy_strong, ptr @__swift_memcpy8_8, ptr @__swift_assignWithTake_strong, ptr @"$sBowet", ptr @"$sBowst", i64 8, i64 8, i32 65543, i32 2147483647, ptr @"$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ" }

// The metadata of every class with an instance allocator, declared or
// inherited, has ClassFlags::HasClassAllocator (0x20) and points to the
// table.
// CHECK: @"$s1M4BaseCMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_1M4PoolO, {{.*}}, i32 34, i32 0,
// CHECK: @"$s1M7DerivedCMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_1M4PoolO, {{.*}}, i32 34, i32 0,
// An opted-out class is an ordinary class.
// CHECK: @"$s1M8OptedOutCMf" = internal global {{.*}} ptr @"$sBoWV", {{.*}}, i32 2, i32 0,
// CHECK: @"$s1M5PlainCMf" = internal global {{.*}} ptr @"$sBoWV", {{.*}}, i32 2, i32 0,
// CHECK: @"$s1M4LeafCMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_1M4PoolO, {{.*}}, i32 34, i32 0,

// The enum's method is the witness itself: it's used, so that it's kept even
// if every call to it is inlined.
// CHECK: @llvm.used = {{.*}}ptr @"$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"
// CHECK-LABEL: define {{.*}}swiftcc void @"$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr {{.*}}%0, i64 %1, i64 %2, ptr {{.*}}%3)

// The allocator is called directly; swift_allocObject is not.
// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s1M4BaseCACycfC"(
// CHECK-NOT: @swift_allocObject(
// ONONE: call swiftcc ptr @"$ss22_allocateClassInstanceyBpxm_q_mBwBwtRlzCs0B9AllocatorR_r0_lF"(
// OPT: call swiftcc ptr @"$s1M12poolAllocateySvSi_SitF"(i64 16, i64 8)
// OPT: call swiftcc ptr @swift_initHeapObject(ptr %0, ptr %1, i64 16, i64 7)
// CHECK-NOT: @swift_allocObject(
// CHECK: }

// The deallocating deinit frees through the allocator.
// CHECK-LABEL: define {{.*}}swiftcc void @"$s1M4BaseCfD"(
// CHECK-NOT: @swift_deallocClassInstance(
// ONONE: call swiftcc void @"$ss24_deallocateClassInstanceyyxm_q_mBpBwBwtRlzCs0B9AllocatorR_r0_lF"(
// OPT: [[ALLOCATION:%.*]] = tail call swiftcc ptr @swift_deallocClassInstanceBegin(ptr %0)
// OPT: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr nonnull [[ALLOCATION]])
// CHECK-NOT: @swift_deallocClassInstance(
// CHECK: }

// ONONE-LABEL: define {{.*}}swiftcc void @"$s1M7DerivedCfD"(
// ONONE-NOT: @swift_deallocClassInstance(
// ONONE: call swiftcc void @"$ss24_deallocateClassInstanceyyxm_q_mBpBwBwtRlzCs0B9AllocatorR_r0_lF"(
// ONONE: }

// ONONE-LABEL: define {{.*}}swiftcc void @"$s1M8OptedOutCfD"(
// ONONE: call void @swift_deallocClassInstance(
// ONONE: }

// ONONE-LABEL: define {{.*}}swiftcc void @"$s1M5PlainCfD"(
// ONONE: call void @swift_deallocClassInstance(
// ONONE: }

// The last unowned release of an instance of a final class with an allocator
// frees it through the allocator; others are freed by the runtime.
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M13UnownedHolderCfd"(
// CHECK: [[ALLOCATION:%.*]] = {{(tail )?}}call ptr @swift_unownedReleaseBegin(ptr [[LEAF:%.*]])
// ONONE: [[IS_LAST:%.*]] = icmp ne ptr [[ALLOCATION]], null
// ONONE: br i1 [[IS_LAST]], label %unowned.free, label %unowned.released
// ONONE: unowned.free:
// ONONE: [[METADATA:%.*]] = load ptr, ptr [[LEAF]]
// The class is final and its layout is fixed: its size and alignment mask are
// constants, as in its deallocating deinit.
// ONONE-NOT: load
// ONONE: call swiftcc void @"$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr [[ALLOCATION]], i64 16, i64 8, ptr [[METADATA]])
// With optimization, the allocator is inlined.
// OPT: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr nonnull [[ALLOCATION]])
// CHECK: unowned.released:
// CHECK: call void @swift_unownedRelease(
// CHECK: }

// So are those of a final generic class whose layout doesn't depend on its
// generic arguments. Those of one whose layout does are loaded from the
// instance's metadata. The metadata passed to the allocator is the instance's.
// ONONE-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M20GenericUnownedHolderCfd"(
// ONONE: [[ALLOCATION:%.*]] = call ptr @swift_unownedReleaseBegin(ptr [[FIXED:%.*]])
// ONONE: unowned.free:
// ONONE: [[METADATA:%.*]] = load ptr, ptr [[FIXED]]
// ONONE-NOT: load
// ONONE: call swiftcc void @"$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr [[ALLOCATION]], i64 24, i64 8, ptr [[METADATA]])
// ONONE: [[ALLOCATION:%.*]] = call ptr @swift_unownedReleaseBegin(ptr [[BOX:%.*]])
// ONONE: unowned.free{{[0-9]*}}:
// ONONE: [[METADATA:%.*]] = load ptr, ptr [[BOX]]
// ONONE: [[SIZE_ADDR:%.*]] = getelementptr inbounds i8, ptr [[METADATA]], i32 {{[0-9]+}}
// ONONE: [[SIZE32:%.*]] = load i32, ptr [[SIZE_ADDR]]
// ONONE: [[SIZE:%.*]] = zext i32 [[SIZE32]] to i64
// ONONE: [[ALIGN_ADDR:%.*]] = getelementptr inbounds i8, ptr [[METADATA]], i32 {{[0-9]+}}
// ONONE: [[ALIGN16:%.*]] = load i16, ptr [[ALIGN_ADDR]]
// ONONE: [[ALIGNMASK:%.*]] = zext i16 [[ALIGN16]] to i64
// ONONE: [[ALIGN:%.*]] = add i64 [[ALIGNMASK]], 1
// ONONE: call swiftcc void @"$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr [[ALLOCATION]], i64 [[SIZE]], i64 [[ALIGN]], ptr [[METADATA]])
// ONONE: }

// The witness of a class allocator, whose `self` is a thick metatype, of a
// generic allocator, and of a default implementation, which can't be the
// allocator's method, is a function with the same signature that calls it.
// CHECK-LABEL: define linkonce_odr hidden swiftcc void @__swift_class_allocator_deallocate_1M17ClassAllocatorBoxC(ptr %0, i64 %1, i64 %2, ptr %3)
// ONONE: call swiftcc void @"$s1M17ClassAllocatorBoxC10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr %0, i64 %1, i64 %2, ptr %3, ptr swiftself {{%.*}})
// OPT: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr %0)
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @__swift_class_allocator_deallocate_1M11GenericPoolVySiG(ptr %0, i64 %1, i64 %2, ptr %3)
// ONONE: call swiftcc void @"$s1M11GenericPoolV10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr %0, i64 %1, i64 %2, ptr %3, ptr @"$sSiN")
// OPT: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr %0)
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @__swift_class_allocator_deallocate_1M11DefaultPoolO(ptr %0, i64 %1, i64 %2, ptr %3)
// ONONE: call swiftcc void @"$ss14ClassAllocatorP1MAC14DefaultingPoolRzrlE10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr %0, i64 %1, i64 %2, ptr %3, ptr {{.*}}, ptr {{.*}}, ptr {{.*}})
// OPT: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr %0)
// CHECK: }

// There are no witnesses for SystemClassAllocator, which no class allocator
// witness table uses, and no function for Pool, whose witness is its method.
// CHECK-NOT: @__swift_class_allocator_{{.*}}SystemClassAllocator
// CHECK-NOT: define {{.*}}@__swift_class_allocator_deallocate_1M4PoolO
