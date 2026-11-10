// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -O -module-name M -emit-sil -sil-verify-all %s | %FileCheck %s

// The rewrite is mandatory: without optimization, it happens before IRGen.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -Onone -module-name M -emit-lowered-sil -sil-verify-all %s | %FileCheck %s --check-prefix=ONONE

// Clients of a module built without library evolution call the allocator
// directly too, after inlining an initializer.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -O -module-name M -emit-module -emit-module-path %t/M.swiftmodule %s
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -O -module-name Client -emit-sil -sil-verify-all -I %t -D CLIENT %s | %FileCheck %s --check-prefix=CLIENT

// REQUIRES: swift_feature_ClassAllocators

#if CLIENT

import M

// CLIENT-LABEL: sil @$s6Client10makeRemote1M4NodeCyF :
// CLIENT-NOT: alloc_ref
// CLIENT: function_ref @$s1M12poolAllocateySvSi_SitF
// CLIENT-NOT: alloc_ref
// CLIENT: } // end sil function '$s6Client10makeRemote1M4NodeCyF'
public func makeRemote() -> Node {
  return Node(1)
}

@inline(never)
func useRemote(_ node: Node) -> Int {
  return node.value
}

public func stackRemote() -> Int {
  let node = Node(5)
  return useRemote(node)
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
// ONONE-LABEL: sil [exact_self_class] @$s1M4NodeCyACSicfC :
// ONONE-NOT: alloc_ref
// ONONE: function_ref @$ss22_allocateClassInstanceyBpxm_q_mBwBwtRlzCs0B9AllocatorR_r0_lF
// ONONE: raw_pointer_to_ref
// ONONE-NOT: alloc_ref
// ONONE: } // end sil function '$s1M4NodeCyACSicfC'
// ONONE-LABEL: sil @$s1M4NodeCfD :
// ONONE-NOT: dealloc_ref
// ONONE: function_ref @$ss24_deallocateClassInstanceyyxm_q_mBpBwBwtRlzCs0B9AllocatorR_r0_lF
// ONONE-NOT: dealloc_ref
// ONONE: } // end sil function '$s1M4NodeCfD'

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

@inline(never)
func use(_ node: Node) -> Int {
  return node.value
}

// An instance that doesn't escape but can't be removed is stack-promoted and
// never reaches the allocator. Its release is devirtualized into the
// destroying deinit (inlined here) and a dealloc_ref, which IRGen drops if it
// puts the object on the stack: neither is rewritten.
// CHECK-LABEL: sil @$s1M14stackCandidateSiyF :
// CHECK-NOT: poolAllocate
// CHECK: [[OBJ:%.*]] = alloc_ref [stack] $Node
// CHECK-NOT: poolAllocate
// CHECK-NOT: swift_deallocClassInstanceBegin
// CHECK-NOT: poolDeallocate
// CHECK: [[DEALLOC:%.*]] = begin_dealloc_ref {{%.*}} of [[OBJ]]
// CHECK-NEXT: dealloc_ref [[DEALLOC]]
// CHECK-NEXT: dealloc_stack_ref [[OBJ]]
// CHECK: } // end sil function '$s1M14stackCandidateSiyF'
public func stackCandidate() -> Int {
  let node = Node(3)
  return use(node)
}

// The destroying deinit isn't inlined: it's called before the dealloc_ref.
@allocator(Pool)
public final class Logged {
  public var value: Int
  public init(_ value: Int) { self.value = value }
  deinit { print("deinit", value) }
}

@inline(never)
func use(_ logged: Logged) -> Int {
  return logged.value
}

// CHECK-LABEL: sil @$s1M15stackWithDeinitSiyF :
// CHECK: [[OBJ:%.*]] = alloc_ref [stack] $Logged
// CHECK: [[DEALLOC:%.*]] = begin_dealloc_ref {{%.*}} of [[OBJ]]
// CHECK: [[DESTROY:%.*]] = function_ref @$s1M6LoggedCfd :
// CHECK: apply [[DESTROY]]([[DEALLOC]])
// CHECK-NEXT: dealloc_ref [[DEALLOC]]
// CHECK-NEXT: dealloc_stack_ref [[OBJ]]
// CHECK: } // end sil function '$s1M15stackWithDeinitSiyF'
public func stackWithDeinit() -> Int {
  let logged = Logged(4)
  return use(logged)
}

// In a whole-module compilation, the destroying deinit of an internal class is
// kept until the release is devirtualized, although its deallocating deinit
// inlines it.
@allocator(Pool)
final class Internal {
  var value: Int
  init(_ value: Int) { self.value = value }
}

@inline(never)
func use(_ object: Internal) -> Int {
  return object.value
}

// CHECK-LABEL: sil @$s1M13stackInternalSiyF :
// CHECK: [[OBJ:%.*]] = alloc_ref [stack] $Internal
// CHECK-NOT: swift_deallocClassInstanceBegin
// CHECK-NOT: poolDeallocate
// CHECK: [[DEALLOC:%.*]] = begin_dealloc_ref {{%.*}} of [[OBJ]]
// CHECK-NEXT: dealloc_ref [[DEALLOC]]
// CHECK-NEXT: dealloc_stack_ref [[OBJ]]
// CHECK: } // end sil function '$s1M13stackInternalSiyF'
public func stackInternal() -> Int {
  let object = Internal(6)
  return use(object)
}

// A client of a module built without library evolution stack-promotes an
// instance of the module's class too.
// CLIENT-LABEL: sil @$s6Client11stackRemoteSiyF :
// CLIENT-NOT: poolAllocate
// CLIENT: alloc_ref [stack] $Node
// CLIENT-NOT: poolAllocate
// CLIENT: dealloc_stack_ref
// CLIENT: } // end sil function '$s6Client11stackRemoteSiyF'

#endif
