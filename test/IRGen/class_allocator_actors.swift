// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -emit-ir -module-name M -Onone | %FileCheck %s --check-prefixes=CHECK,ONONE
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -emit-ir -module-name M -O | %FileCheck %s --check-prefixes=CHECK,OPT

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: concurrency
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
public actor DefaultActor {
  public init() {}
}

@allocator(Pool)
public actor ActorOnMainActor {
  public nonisolated var unownedExecutor: UnownedSerialExecutor {
    MainActor.sharedUnownedExecutor
  }
  public init() {}
}

// Actors with allocators have ClassFlags::HasClassAllocator (0x20) and the
// class allocator value witnesses.
// CHECK: @"$s1M12DefaultActorCMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_1M4PoolO, {{.*}}, i32 34, i32 0,
// CHECK: @"$s1M011ActorOnMainA0CMf" = internal global {{.*}} ptr @__swift_class_allocator_value_witnesses_1M4PoolO, {{.*}}, i32 34, i32 0,

// An actor is allocated through its allocator.
// CHECK-LABEL: define {{.*}}swiftcc ptr @"$s1M12DefaultActorCACycfC"(
// CHECK-NOT: @swift_allocObject(
// ONONE: call swiftcc ptr @"$ss22_allocateClassInstanceyBpxm_q_mBwBwtRlzCs0B9AllocatorR_r0_lF"(
// OPT: call swiftcc ptr @"$s1M12poolAllocateySvSi_SitF"(
// OPT: call swiftcc ptr @swift_initHeapObject(
// CHECK: call swiftcc void @swift_defaultActor_initialize(
// CHECK: }

// A default actor's deinit leaves the free to the concurrency runtime, which
// frees it through the allocator, possibly once the actor stops running.
// CHECK-LABEL: define {{.*}}swiftcc void @"$s1M12DefaultActorCfD"(
// CHECK-NOT: @swift_deallocClassInstanceBegin(
// CHECK: call swiftcc void @swift_defaultActor_deallocate(
// CHECK-NOT: @swift_deallocClassInstanceBegin(
// CHECK: }

// The deinit of an actor with a custom executor frees it through the
// allocator directly.
// CHECK-LABEL: define {{.*}}swiftcc void @"$s1M011ActorOnMainA0CfD"(
// CHECK-NOT: @swift_deallocClassInstance(
// ONONE: call swiftcc void @"$ss24_deallocateClassInstanceyyxm_q_mBpBwBwtRlzCs0B9AllocatorR_r0_lF"(
// OPT: [[ALLOCATION:%.*]] = tail call swiftcc ptr @swift_deallocClassInstanceBegin(ptr %0)
// OPT: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr nonnull [[ALLOCATION]])
// CHECK: }


public final class UnownedHolder {
  public unowned let actor: DefaultActor
  public init(_ actor: DefaultActor) {
    self.actor = actor
  }
}

// The last unowned release of an actor with an allocator frees it through the
// allocator.
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M13UnownedHolderCfd"(
// CHECK: call ptr @swift_unownedReleaseBegin(
// CHECK: }
