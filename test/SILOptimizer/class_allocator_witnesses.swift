// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -Onone -module-name M -emit-sil -sil-verify-all %s | %FileCheck %s --check-prefixes=CHECK,ONONE
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -O -module-name M -emit-sil -sil-verify-all %s | %FileCheck %s --check-prefixes=CHECK,OPT
// The pass alone.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name M -emit-silgen %s | %target-sil-opt -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name M -class-allocator-witnesses -sil-verify-all | %FileCheck %s --check-prefixes=CHECK,PASS

// REQUIRES: swift_feature_ClassAllocators

// The value witness table of a class with an allocator ends with a witness
// that frees through its allocator's `deallocate`, with that method's
// signature if the allocator is a non-generic struct or enum. If the
// allocator's witness is such a method, the witness is the method, which is
// marked used. Otherwise it's a shared function, one per allocator type,
// referenced only from IRGen-emitted tables, which calls the allocator's
// witness, directly if it can.

@inline(never)
public func poolAllocate(_ byteCount: Int, _ alignment: Int) -> UnsafeMutableRawPointer {
  UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
}

@inline(never)
public func poolDeallocate(_ pointer: UnsafeMutableRawPointer) {
  unsafe pointer.deallocate()
}

public enum Pool: @unsafe ClassAllocator {
  @inlinable
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  @inlinable
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

// A method without a symbol in its module.
public struct EmittedIntoClientPool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  @_alwaysEmitIntoClient
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

// Its `self` is a thick metatype.
public final class FinalClassPool: @unsafe ClassAllocator {
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

// Its `deallocate` is dispatched through its vtable.
open class OverridablePool: @unsafe ClassAllocator {
  open class func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  open class func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

@allocator(Pool)
public final class A {}

@allocator(Pool)
public final class B {}

public final class C {}

@allocator(EmittedIntoClientPool)
public final class D {}

@allocator(FinalClassPool)
public final class E {}

@allocator(OverridablePool)
public final class F {}

// CHECK-LABEL: sil {{.*}}[used] {{.*}}@$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ : $@convention(method) (UnsafeMutableRawPointer, Int, Int, @thick any AnyObject.Type, @thin Pool.Type) -> () {

// CHECK-LABEL: sil {{.*}}[used] {{.*}}@$s1M21EmittedIntoClientPoolV10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ : $@convention(method) (UnsafeMutableRawPointer, Int, Int, @thick any AnyObject.Type, @thin EmittedIntoClientPool.Type) -> () {

// CHECK-LABEL: sil shared [used] {{(\[ossa\] )?}}@__swift_class_allocator_deallocate_1M14FinalClassPoolC : $@convention(thin) (UnsafeMutableRawPointer, Int, Int, @thick any AnyObject.Type) -> () {
// ONONE-DAG: [[SELF:%.*]] = metatype $@thick FinalClassPool.Type
// ONONE-DAG: [[FN:%.*]] = function_ref @$s1M14FinalClassPoolC10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ
// ONONE: apply [[FN]](%0, %1, %2, %3, [[SELF]])
// PASS-DAG: [[SELF:%.*]] = metatype $@thick FinalClassPool.Type
// PASS-DAG: [[FN:%.*]] = function_ref @$s1M14FinalClassPoolC10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ
// PASS: apply [[FN]](%0, %1, %2, %3, [[SELF]])
// OPT: function_ref @$s1M14poolDeallocateyySvF
// CHECK: } // end sil function '__swift_class_allocator_deallocate_1M14FinalClassPoolC'

// The function calls a method dispatched through the vtable through the
// conformance, which the optimizer devirtualizes, as the metatype is exact.
// CHECK-LABEL: sil shared [used] {{(\[ossa\] )?}}@__swift_class_allocator_deallocate_1M15OverridablePoolC : $@convention(thin) (UnsafeMutableRawPointer, Int, Int, @thick any AnyObject.Type) -> () {
// PASS-DAG: [[FN:%.*]] = witness_method $OverridablePool, #ClassAllocator.deallocate
// PASS-DAG: [[SELF:%.*]] = metatype $@thick OverridablePool.Type
// PASS: apply [[FN]]<OverridablePool>(%0, %1, %2, %3, [[SELF]])
// ONONE: [[FN:%.*]] = function_ref @$s1M15OverridablePoolC10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ
// ONONE: apply [[FN]](%0, %1, %2, %3, {{%.*}})
// OPT: function_ref @$s1M14poolDeallocateyySvF
// CHECK: } // end sil function '__swift_class_allocator_deallocate_1M15OverridablePoolC'

// There is no function for the allocators whose witness is their method, and
// none for SystemClassAllocator.
// CHECK-NOT: sil shared [used] {{.*}}@__swift_class_allocator_deallocate_
