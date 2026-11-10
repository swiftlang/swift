// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -primary-file %s -emit-ir -module-name M -O | %FileCheck %s --check-prefixes=CHECK,STACK
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -primary-file %s -emit-ir -module-name M -O -stack-promotion-limit 0 | %FileCheck %s --check-prefixes=CHECK,HEAP

// A client of a module built without library evolution.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple %s -emit-module -emit-module-path %t/M.swiftmodule -module-name M -O
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -I %t -emit-ir -module-name Client -O -D CLIENT | %FileCheck %s --check-prefix=CLIENT
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -I %t -emit-ir -module-name Client -O -D CLIENT -stack-promotion-limit 0 | %FileCheck %s --check-prefix=CLIENT-HEAP

// With cross-module optimization, the client inlines the module's
// deallocating deinit, which frees through the allocator.
// RUN: %empty-directory(%t/cmo)
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple %s -emit-module -emit-module-path %t/cmo/M.swiftmodule -module-name M -O -cross-module-optimization
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -I %t/cmo -emit-ir -module-name Client -O -D CLIENT | %FileCheck %s --check-prefix=CLIENT-CMO
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -I %t/cmo -emit-ir -module-name Client -O -D CLIENT -stack-promotion-limit 0 | %FileCheck %s --check-prefix=CLIENT-CMO-HEAP

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: CPU=arm64 || CPU=x86_64

// A stack-promoted instance of a class with a class allocator never reaches
// the allocator. If IRGen can't put it on the stack (because of the stack
// budget, here -stack-promotion-limit 0, or a class layout that isn't fixed),
// it allocates it with the allocator and swift_initHeapObject, never with
// swift_allocObject, and frees it through the allocator.

#if CLIENT

import M

@inline(never)
func useInClient(_ node: Node) -> Int {
  return node.value
}

// The destroying deinit of M.Node isn't visible, so the devirtualized release
// calls the deallocating deinit, which frees only heap objects through the
// allocator.
// CLIENT-LABEL: define {{.*}}swiftcc i64 @"$s6Client11clientLocalyS2iF"(
// CLIENT-NOT: call
// CLIENT: %reference.raw = alloca %T1M4NodeC
// CLIENT-NOT: poolAllocate
// CLIENT-NOT: swift_initHeapObject
// CLIENT-NOT: swift_allocObject
// CLIENT: call ptr @swift_initStackObject({{.*}}%reference.raw)
// CLIENT-NOT: poolAllocate
// CLIENT: call swiftcc void @"$s1M4NodeCfD"(
// CLIENT: ret i64

// CLIENT-HEAP-LABEL: define {{.*}}swiftcc i64 @"$s6Client11clientLocalyS2iF"(
// CLIENT-HEAP-NOT: swift_allocObject
// CLIENT-HEAP-NOT: swift_initStackObject
// CLIENT-HEAP: [[MEM:%.*]] = tail call swiftcc ptr @"$s1M12poolAllocateySvSi_SitF"(i64 24, i64 8)
// CLIENT-HEAP: tail call swiftcc ptr @swift_initHeapObject(ptr {{%.*}}, ptr [[MEM]], i64 24, i64 7)
// CLIENT-HEAP-NOT: swift_allocObject
// CLIENT-HEAP: call swiftcc void @"$s1M4NodeCfD"(
// CLIENT-HEAP: ret i64
// CLIENT-CMO-LABEL: define {{.*}}swiftcc i64 @"$s6Client11clientLocalyS2iF"(
// CLIENT-CMO: %reference.raw = alloca %T1M4NodeC
// CLIENT-CMO: call ptr @swift_initStackObject({{.*}}%reference.raw)
// CLIENT-CMO-NEXT: getelementptr
// CLIENT-CMO-NEXT: store
// CLIENT-CMO-NEXT: call swiftcc i64 @"$s6Client05useInA0ySi1M4NodeCF"(
// CLIENT-CMO-NEXT: ret i64

// The heap object is freed once, by the dealloc_ref that replaces the inlined
// deinit's free.
// CLIENT-CMO-HEAP-LABEL: define {{.*}}swiftcc i64 @"$s6Client11clientLocalyS2iF"(
// CLIENT-CMO-HEAP-NOT: swift_allocObject
// CLIENT-CMO-HEAP: call swiftcc ptr @"$s1M12poolAllocateySvSi_SitF"(i64 24, i64 8)
// CLIENT-CMO-HEAP: call swiftcc ptr @swift_initHeapObject(
// CLIENT-CMO-HEAP: call void @swift_setDeallocating(
// CLIENT-CMO-HEAP: call swiftcc ptr @swift_deallocClassInstanceBegin(
// CLIENT-CMO-HEAP-NOT: swift_deallocClassInstanceBegin
// IRGen calls the allocator's deallocate witness, which is its method, with
// the alignment, not the alignment mask.
// CLIENT-CMO-HEAP: call swiftcc void @"$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr nonnull {{%.*}}, i64 24, i64 8, ptr {{%.*}})
// CLIENT-CMO-HEAP-NOT: poolDeallocate
// CLIENT-CMO-HEAP-NOT: Pool{{.*}}deallocate
// CLIENT-CMO-HEAP: ret i64
public func clientLocal(_ x: Int) -> Int {
  let node = Node(x)
  return useInClient(node)
}

// The client has the allocator's allocate witness, for the heap fallback. Its
// deallocate witness is the allocator's method.
// CLIENT: define linkonce_odr hidden swiftcc ptr @__swift_class_allocator_allocate_1M4PoolO(
// CLIENT-HEAP: define linkonce_odr hidden swiftcc ptr @__swift_class_allocator_allocate_1M4PoolO(
// CLIENT-NOT: @__swift_class_allocator_deallocate_
// CLIENT-HEAP-NOT: @__swift_class_allocator_deallocate_

#else

import Builtin

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

@inline(never)
func use(_ node: Node) -> Int {
  return node.value
}

// On the stack, the object costs what an instance of a class without an
// allocator costs: its devirtualized release and inlined deinit leave nothing.
// STACK-LABEL: define {{.*}}swiftcc i64 @"$s1M10stackLocalyS2iF"(
// STACK: %reference.raw = alloca %T1M4NodeC
// STACK-NOT: poolAllocate
// STACK-NOT: swift_initHeapObject
// STACK-NOT: swift_allocObject
// STACK: call ptr @swift_initStackObject({{.*}}%reference.raw)
// STACK-NOT: swift_setDeallocating
// STACK-NOT: swift_deallocClassInstanceBegin
// STACK-NOT: poolDeallocate
// STACK-NOT: swift_release
// STACK: ret i64

// HEAP-LABEL: define {{.*}}swiftcc i64 @"$s1M10stackLocalyS2iF"(
// HEAP-NOT: swift_allocObject
// HEAP-NOT: swift_initStackObject
// HEAP: [[MEM:%.*]] = tail call swiftcc ptr @"$s1M12poolAllocateySvSi_SitF"(i64 24, i64 8)
// HEAP: [[OBJ:%.*]] = tail call swiftcc ptr @swift_initHeapObject(ptr {{%.*}}, ptr [[MEM]], i64 24, i64 7)
// HEAP: call swiftcc i64 @"$s1M3useySiAA4NodeCF"(ptr [[OBJ]])
// HEAP: call void @swift_setDeallocating(ptr [[OBJ]])
// HEAP: [[FREE:%.*]] = tail call swiftcc ptr @swift_deallocClassInstanceBegin(ptr [[OBJ]])
// HEAP: icmp eq ptr [[FREE]], null
// HEAP: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr nonnull [[FREE]])
// HEAP-NOT: swift_allocObject
// HEAP: ret i64
public func stackLocal(_ x: Int) -> Int {
  let node = Node(x)
  return use(node)
}

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

// With a deinit that isn't inlined, the destroying deinit is called directly.
// STACK-LABEL: define {{.*}}swiftcc i64 @"$s1M15stackWithDeinityS2iF"(
// STACK: %reference.raw = alloca %T1M6LoggedC
// STACK-NOT: poolAllocate
// STACK: call ptr @swift_initStackObject({{.*}}%reference.raw)
// STACK: call void @swift_setDeallocating(
// STACK: call swiftcc ptr @"$s1M6LoggedCfd"(
// STACK-NOT: swift_deallocClassInstanceBegin
// STACK-NOT: poolDeallocate
// STACK: ret i64

// HEAP-LABEL: define {{.*}}swiftcc i64 @"$s1M15stackWithDeinityS2iF"(
// HEAP-NOT: swift_allocObject
// HEAP: call swiftcc ptr @"$s1M12poolAllocateySvSi_SitF"(
// HEAP: call swiftcc ptr @swift_initHeapObject(
// HEAP: call swiftcc ptr @"$s1M6LoggedCfd"(
// HEAP: call swiftcc ptr @swift_deallocClassInstanceBegin(
// HEAP: call swiftcc void @"$s1M14poolDeallocateyySvF"(
// HEAP: ret i64
public func stackWithDeinit(_ x: Int) -> Int {
  let logged = Logged(x)
  return use(logged)
}

@allocator(Pool)
public final class Box<T> {
  public var value: T
  @inline(__always)
  public init(_ value: T) { self.value = value }
}

@inline(never)
func use<T>(_ box: Box<T>) -> Int {
  return 1
}

// The layout of Box<T> isn't fixed, so IRGen always allocates it with the
// allocator, with the size and alignment from its metadata.
// CHECK-LABEL: define {{.*}}swiftcc i64 @"$s1M12genericLocalySixlF"(
// CHECK-NOT: swift_allocObject
// CHECK-NOT: swift_initStackObject
// CHECK: [[SIZE32:%.*]] = load i32, ptr
// CHECK: [[SIZE:%.*]] = zext i32 [[SIZE32]] to i64
// CHECK: [[ALIGNMASK16:%.*]] = load i16, ptr
// CHECK: [[ALIGNMASK:%.*]] = zext i16 [[ALIGNMASK16]] to i64
// CHECK: [[ALIGN:%.*]] = add nuw nsw i64 [[ALIGNMASK]], 1
// CHECK: [[MEM:%.*]] = tail call swiftcc ptr @"$s1M12poolAllocateySvSi_SitF"(i64 [[SIZE]], i64 [[ALIGN]])
// CHECK: [[OBJ:%.*]] = tail call swiftcc ptr @swift_initHeapObject(ptr {{%.*}}, ptr [[MEM]], i64 [[SIZE]], i64 [[ALIGNMASK]])
// CHECK: [[FREE:%.*]] = tail call swiftcc ptr @swift_deallocClassInstanceBegin(ptr {{.*}}[[OBJ]])
// CHECK: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr nonnull [[FREE]])
// CHECK-NOT: swift_allocObject
// CHECK: ret i64
@_semantics("optimize.sil.specialize.generic.never")
@inline(never)
public func genericLocal<T>(_ x: T) -> Int {
  let box = Box(x)
  return use(box)
}

@allocator(Pool)
public final class Table {
  public var a: Int, b: Int
  init(a: Int, b: Int) { self.a = a; self.b = b }
}

@inline(never)
func read(_ table: Table, _ i: Int) -> Int {
  return i == 0 ? table.a : table.b
}

// A stack-promoted object that isn't mutated after its initialization may be
// outlined into a statically initialized object, which never reaches the
// allocator either.
// CHECK-LABEL: define {{.*}}swiftcc i64 @"$s1M6lookupyS2iF"(
// CHECK-NOT: poolAllocate
// CHECK-NOT: swift_allocObject
// CHECK: call ptr @swift_initStaticObject(
// CHECK-NOT: poolAllocate
// CHECK-NOT: swift_allocObject
// CHECK: ret i64
@inline(never)
public func lookup(_ i: Int) -> Int {
  var table = Table(a: 10, b: 20)
  Builtin.endCOWMutation(&table)
  return read(table, i)
}

// The allocator's witnesses, for the heap fallback.
// CHECK: define linkonce_odr hidden swiftcc ptr @__swift_class_allocator_allocate_1M4PoolO(ptr %0, i64 %1, i64 %2)
// CHECK: [[ALIGN:%.*]] = add i64 %2, 1
// CHECK: call swiftcc ptr @"$s1M12poolAllocateySvSi_SitF"(i64 %1, i64 [[ALIGN]])

#endif
