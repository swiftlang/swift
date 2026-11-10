// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -target %target-future-triple -Onone -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -target %target-future-triple -O -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -target %target-future-triple -O -wmo -Xfrontend -sil-verify-all)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_ClassAllocators
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

// The runtime frees instances through the `deallocate` class allocator
// witness: after the last unowned release of a reference whose static type
// is a class that may have subclasses outside the module, when an
// initializer fails before the object is initialized
// (swift_deallocPartialClassInstance), and when a default actor is
// deinitialized. The witness
// is the allocator's method for an enum allocator, and a function that calls
// the allocator for a class allocator and a generic allocator. Either way the
// allocator gets the pointer it returned, and the byte count, alignment (not
// alignment mask) and class it was asked for.

import StdlibUnittest

struct Event: Equatable {
  var pool: String
  var pointer: UnsafeMutableRawPointer
  var byteCount: Int
  var alignment: Int
  var type: ObjectIdentifier
}

nonisolated(unsafe) var allocations: [Event] = []
nonisolated(unsafe) var deallocations: [Event] = []

func recordAllocation(
  _ pool: String, _ byteCount: Int, _ alignment: Int, _ type: AnyClass
) -> UnsafeMutableRawPointer {
  let pointer = UnsafeMutableRawPointer.allocate(
    byteCount: byteCount, alignment: alignment)
  unsafe allocations.append(Event(
    pool: pool, pointer: pointer, byteCount: byteCount, alignment: alignment,
    type: ObjectIdentifier(type)))
  return unsafe pointer
}

func recordDeallocation(
  _ pool: String, _ pointer: UnsafeMutableRawPointer, _ byteCount: Int,
  _ alignment: Int, _ type: AnyClass
) {
  unsafe deallocations.append(Event(
    pool: pool, pointer: pointer, byteCount: byteCount, alignment: alignment,
    type: ObjectIdentifier(type)))
  unsafe pointer.deallocate()
}

// The witness is this method.
public enum EnumPool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe recordAllocation("enum", byteCount, alignment, type)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe recordDeallocation("enum", pointer, byteCount, alignment, type)
  }
}

// The witness is a function that calls these methods.
public final class ClassPool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe recordAllocation("class", byteCount, alignment, type)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe recordDeallocation("class", pointer, byteCount, alignment, type)
  }
}

public struct GenericPool<T>: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe recordAllocation("generic", byteCount, alignment, type)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe recordDeallocation("generic", pointer, byteCount, alignment, type)
  }
}

struct InitFailure: Error {}

nonisolated(unsafe) var escaped: AnyObject?

@inline(never)
func escape(_ object: AnyObject?) {
  escaped = object
}

// `open`, so that the last unowned release of a reference of this type, even
// in a whole-module compilation, leaves the free to the runtime.
@allocator(EnumPool)
open class EnumBase {
  var x: Int
  public init() { x = 1 }
  // Fails before `x` is initialized: the object is partially initialized.
  @inline(never)
  init(fail: Bool) throws {
    if fail { throw InitFailure() }
    x = 1
  }
}
final class EnumDerived: EnumBase {
  var vector = SIMD4<Double>(repeating: 1)
}
final class EnumFailing: EnumBase {
  var y: Int
  // Fails before the superclass is initialized.
  @inline(never)
  init(failBeforeSuper: Bool) throws {
    y = 2
    if failBeforeSuper { throw InitFailure() }
    super.init()
  }
}
@allocator(EnumPool)
actor EnumActor {
  var value = 0
}

@allocator(ClassPool)
open class ClassBase {
  var x: Int
  public init() { x = 1 }
  // Fails before `x` is initialized: the object is partially initialized.
  @inline(never)
  init(fail: Bool) throws {
    if fail { throw InitFailure() }
    x = 1
  }
}
final class ClassDerived: ClassBase {
  var vector = SIMD4<Double>(repeating: 1)
}
final class ClassFailing: ClassBase {
  var y: Int
  // Fails before the superclass is initialized.
  @inline(never)
  init(failBeforeSuper: Bool) throws {
    y = 2
    if failBeforeSuper { throw InitFailure() }
    super.init()
  }
}
@allocator(ClassPool)
actor ClassActor {
  var value = 0
}

@allocator(GenericPool<Int>)
open class GenericBase {
  var x: Int
  public init() { x = 1 }
  // Fails before `x` is initialized: the object is partially initialized.
  @inline(never)
  init(fail: Bool) throws {
    if fail { throw InitFailure() }
    x = 1
  }
}
final class GenericDerived: GenericBase {
  var vector = SIMD4<Double>(repeating: 1)
}
final class GenericFailing: GenericBase {
  var y: Int
  // Fails before the superclass is initialized.
  @inline(never)
  init(failBeforeSuper: Bool) throws {
    y = 2
    if failBeforeSuper { throw InitFailure() }
    super.init()
  }
}
@allocator(GenericPool<Int>)
actor GenericActor {
  var value = 0
}

final class UnownedEnumHolder {
  unowned let target: EnumBase
  init(_ target: EnumBase) { self.target = target }
}
final class UnownedClassHolder {
  unowned let target: ClassBase
  init(_ target: ClassBase) { self.target = target }
}
final class UnownedGenericHolder {
  unowned let target: GenericBase
  init(_ target: GenericBase) { self.target = target }
}

@inline(never)
func makeEnumDerived() -> EnumBase { EnumDerived() }
@inline(never)
func makeClassDerived() -> ClassBase { ClassDerived() }
@inline(never)
func makeGenericDerived() -> GenericBase { GenericDerived() }

/// Runs `body`, which must allocate and free one instance of `type` with
/// `pool`, and checks that the free returned what the allocation got.
func expectFreedAsAllocated(
  _ pool: String, _ type: AnyClass, _ body: () -> Void,
  file: String = #file, line: UInt = #line
) {
  allocations = []
  deallocations = []
  body()
  let loc = SourceLoc(file, line)
  expectEqual(allocations.count, 1, stackTrace: SourceLocStack().with(loc))
  expectEqual(deallocations.count, 1, stackTrace: SourceLocStack().with(loc))
  guard allocations.count == 1, deallocations.count == 1 else { return }
  let allocation = allocations[0], deallocation = deallocations[0]
  expectEqual(allocation.pool, pool, stackTrace: SourceLocStack().with(loc))
  expectEqual(allocation.type, ObjectIdentifier(type),
              stackTrace: SourceLocStack().with(loc))
  // The alignment is a power of two, not a mask.
  expectEqual(allocation.alignment & (allocation.alignment - 1), 0,
              stackTrace: SourceLocStack().with(loc))
  expectEqual(deallocation, allocation, stackTrace: SourceLocStack().with(loc))
}

var tests = TestSuite("ClassAllocatorDeallocateWitness")

tests.test("last unowned release through a class with outside subclasses") {
  func check<Holder: AnyObject, Base: AnyObject>(
    _ pool: String, _ type: AnyClass, _ make: () -> Base,
    _ hold: (Base) -> Holder
  ) {
    expectFreedAsAllocated(pool, type) {
      var object: Base? = make()
      var holder: Holder? = hold(object!)
      escape(holder)
      object = nil
      // Deinitialized; the memory stays allocated for the unowned reference.
      expectEqual(deallocations.count, 0)
      holder = nil
      escape(nil)
      _ = object
      _ = holder
    }
    // The alignment of the instance's SIMD4<Double> field.
    expectEqual(allocations.first?.alignment,
                MemoryLayout<SIMD4<Double>>.alignment)
  }
  check("enum", EnumDerived.self, makeEnumDerived, UnownedEnumHolder.init)
  check("class", ClassDerived.self, makeClassDerived, UnownedClassHolder.init)
  check("generic", GenericDerived.self, makeGenericDerived,
        UnownedGenericHolder.init)
}

tests.test("initializers that fail before the object is initialized") {
  expectFreedAsAllocated("enum", EnumBase.self) {
    _ = try? EnumBase(fail: true)
  }
  expectFreedAsAllocated("enum", EnumFailing.self) {
    _ = try? EnumFailing(failBeforeSuper: true)
  }
  expectFreedAsAllocated("class", ClassBase.self) {
    _ = try? ClassBase(fail: true)
  }
  expectFreedAsAllocated("class", ClassFailing.self) {
    _ = try? ClassFailing(failBeforeSuper: true)
  }
  expectFreedAsAllocated("generic", GenericBase.self) {
    _ = try? GenericBase(fail: true)
  }
  expectFreedAsAllocated("generic", GenericFailing.self) {
    _ = try? GenericFailing(failBeforeSuper: true)
  }
}

// An idle default actor is freed by the concurrency runtime as soon as its
// deinit finishes.
tests.test("default actors") {
  expectFreedAsAllocated("enum", EnumActor.self) {
    escape(EnumActor())
    escape(nil)
  }
  expectFreedAsAllocated("class", ClassActor.self) {
    escape(ClassActor())
    escape(nil)
  }
  expectFreedAsAllocated("generic", GenericActor.self) {
    escape(GenericActor())
    escape(nil)
  }
}

runAllTests()
