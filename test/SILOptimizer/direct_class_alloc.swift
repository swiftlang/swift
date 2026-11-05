// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -O -module-name M -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -O -module-name M -emit-sil %s -Xllvm -sil-disable-direct-class-alloc | %FileCheck %s --check-prefix=DISABLED

// Other modules use the runtime path, even after inlining an initializer.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -O -module-name M -emit-module -emit-module-path %t/M.swiftmodule %s
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -O -module-name Client -emit-sil -I %t -D CLIENT %s | %FileCheck %s --check-prefix=CLIENT

// REQUIRES: swift_feature_ClassAllocators

#if CLIENT

import M

// CLIENT-LABEL: sil @$s6Client10makeRemote1M4NodeCyF :
// CLIENT-NOT: poolAllocate
// CLIENT: alloc_ref $Node
// CLIENT-NOT: poolAllocate
// CLIENT: } // end sil function '$s6Client10makeRemote1M4NodeCyF'
public func makeRemote() -> Node {
  return Node(1)
}

#else

@inline(never) @usableFromInline
func poolAllocate(_ byteCount: Int, _ alignment: Int) -> UnsafeMutableRawPointer {
  UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
}

@inline(never) @usableFromInline
func poolDeallocate(_ pointer: UnsafeMutableRawPointer) {
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

@allocator(Pool)
public final class Node {
  public var value: Int
  @inlinable
  public init(_ value: Int) { self.value = value }
}

// The deallocating deinit frees through the allocator, once the runtime has
// done its bookkeeping.
// CHECK-LABEL: sil @$s1M4NodeCfD :
// CHECK-NOT: dealloc_ref
// CHECK: [[BEGIN:%.*]] = function_ref @swift_deallocClassInstanceBegin
// CHECK: apply [[BEGIN]](
// CHECK: switch_enum
// CHECK: [[FREE:%.*]] = function_ref @$s1M14poolDeallocateyySvF
// CHECK: apply [[FREE]](
// CHECK-NOT: dealloc_ref
// CHECK: } // end sil function '$s1M4NodeCfD'
// DISABLED-LABEL: sil @$s1M4NodeCfD :
// DISABLED: dealloc_ref
// DISABLED: } // end sil function '$s1M4NodeCfD'

public class Base {}

// A class without an allocator is unchanged.
// CHECK-LABEL: sil @$s1M4BaseCfD :
// CHECK: dealloc_ref
// CHECK: } // end sil function '$s1M4BaseCfD'

@allocator(Pool)
public final class Leaf: Base {}

// CHECK-LABEL: sil @$s1M4LeafCfD :
// CHECK-NOT: dealloc_ref
// CHECK: function_ref @$s1M14poolDeallocateyySvF
// CHECK: } // end sil function '$s1M4LeafCfD'

@allocator(Pool)
public final class Box<T> {
  public var value: T
  public init(_ value: T) { self.value = value }
}

// Unspecialized generic code calls the allocator directly too.
// CHECK-LABEL: sil [exact_self_class] @$s1M3BoxCyACyxGxcfC :
// CHECK-NOT: alloc_ref
// CHECK: function_ref @$s1M12poolAllocateySvSi_SitF
// CHECK: raw_pointer_to_ref {{%.*}} to $Box<T>
// CHECK: } // end sil function '$s1M3BoxCyACyxGxcfC'

// CHECK-LABEL: sil @$s1M3BoxCfD :
// CHECK-NOT: dealloc_ref
// CHECK: function_ref @swift_deallocClassInstanceBegin
// CHECK: function_ref @$s1M14poolDeallocateyySvF
// CHECK: } // end sil function '$s1M3BoxCfD'

// The allocator is called directly; swift_allocObject is not.
// CHECK-LABEL: sil @$s1M8makeNodeAA0B0CyF :
// CHECK-NOT: alloc_ref
// CHECK: [[ALLOC:%.*]] = function_ref @$s1M12poolAllocateySvSi_SitF
// CHECK: [[MEM:%.*]] = apply [[ALLOC]](
// CHECK: [[INIT:%.*]] = function_ref @swift_initHeapObject
// CHECK: apply [[INIT]]({{%[0-9]+}}, [[MEM]], {{%[0-9]+}}, {{%[0-9]+}})
// CHECK: raw_pointer_to_ref
// CHECK-NOT: alloc_ref
// CHECK: } // end sil function '$s1M8makeNodeAA0B0CyF'
// DISABLED-LABEL: sil @$s1M8makeNodeAA0B0CyF :
// DISABLED: alloc_ref $Node
// DISABLED: } // end sil function '$s1M8makeNodeAA0B0CyF'
public func makeNode() -> Node {
  return Node(1)
}

// A subclass with an allocator.
// CHECK-LABEL: sil @$s1M8makeLeafAA0B0CyF :
// CHECK-NOT: alloc_ref
// CHECK: function_ref @$s1M12poolAllocateySvSi_SitF
// CHECK: } // end sil function '$s1M8makeLeafAA0B0CyF'
public func makeLeaf() -> Leaf {
  return Leaf()
}

// A specialized generic class.
// CHECK-LABEL: sil @$s1M7makeBoxAA0B0CySiGyF :
// CHECK-NOT: alloc_ref
// CHECK: function_ref @$s1M12poolAllocateySvSi_SitF
// CHECK: } // end sil function '$s1M7makeBoxAA0B0CySiGyF'
public func makeBox() -> Box<Int> {
  return Box(1)
}

// A non-escaping instance is optimized away (or stays on the stack) and never
// reaches the allocator.
// CHECK-LABEL: sil @$s1M10stackLocalSiyF :
// CHECK-NOT: poolAllocate
// CHECK: } // end sil function '$s1M10stackLocalSiyF'
public func stackLocal() -> Int {
  let node = Node(2)
  return node.value + 1
}

#endif
