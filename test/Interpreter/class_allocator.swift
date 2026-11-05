// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -target %target-future-triple -Onone)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -target %target-future-triple -O)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -target %target-future-triple -O -Xllvm -sil-disable-direct-class-alloc)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ClassAllocators
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import Synchronization

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

// A malloc-backed allocator that tags its memory, so that freeing memory it
// did not allocate, or freeing its memory with free(), is detected.
struct Counts: Equatable, CustomStringConvertible {
  var allocations: Int
  var deallocations: Int
  var description: String { "\(allocations)/\(deallocations)" }
}

let headerSize = 32

func tagAndAllocate(
  _ byteCount: Int, _ alignment: Int, _ tag: Int
) -> UnsafeMutableRawPointer {
  precondition(alignment > 0 && alignment & (alignment - 1) == 0)
  let alignment = max(alignment, headerSize)
  let base = UnsafeMutableRawPointer.allocate(
    byteCount: byteCount + alignment, alignment: alignment)
  let memory = unsafe base + alignment
  unsafe (memory - 8).storeBytes(of: tag, as: Int.self)
  unsafe (memory - 16).storeBytes(of: alignment, as: Int.self)
  unsafe (memory - 24).storeBytes(of: byteCount, as: Int.self)
  return unsafe memory
}

func checkTagAndDeallocate(_ memory: UnsafeMutableRawPointer, _ tag: Int) {
  let actualTag = unsafe (memory - 8).load(as: Int.self)
  if actualTag != tag {
    fatalError("freed memory that this allocator did not allocate")
  }
  let alignment = unsafe (memory - 16).load(as: Int.self)
  unsafe (memory - 8).storeBytes(of: 0, as: Int.self)
  unsafe (memory - alignment).deallocate()
}

func allocatedByteCount(_ memory: UnsafeRawPointer) -> Int {
  return unsafe (memory - 24).load(as: Int.self)
}

let poolAllocations = Atomic<Int>(0)
let poolDeallocations = Atomic<Int>(0)
let otherPoolAllocations = Atomic<Int>(0)
let otherPoolDeallocations = Atomic<Int>(0)
let usableSizeCalls = Atomic<Int>(0)
nonisolated(unsafe) var lastAllocatedType: AnyClass?
nonisolated(unsafe) var lastDeallocatedType: AnyClass?
nonisolated(unsafe) var lastAlignment = 0
nonisolated(unsafe) var lastByteCount = 0

enum Pool: @unsafe ClassAllocator {
  static let tag = 0x506f6f6c

  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    poolAllocations.add(1, ordering: .relaxed)
    unsafe lastAllocatedType = type
    unsafe lastAlignment = alignment
    unsafe lastByteCount = byteCount
    return unsafe tagAndAllocate(byteCount, alignment, tag)
  }

  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    poolDeallocations.add(1, ordering: .relaxed)
    unsafe lastDeallocatedType = type
    unsafe checkTagAndDeallocate(pointer, tag)
  }

  static func usableSize(
    of pointer: UnsafeRawPointer, for type: AnyClass
  ) -> Int? {
    usableSizeCalls.add(1, ordering: .relaxed)
    return unsafe allocatedByteCount(pointer)
  }
}

enum OtherPool: @unsafe ClassAllocator {
  static let tag = 0x4f746872

  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    otherPoolAllocations.add(1, ordering: .relaxed)
    return unsafe tagAndAllocate(byteCount, alignment, tag)
  }

  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    otherPoolDeallocations.add(1, ordering: .relaxed)
    unsafe checkTagAndDeallocate(pointer, tag)
  }
}

func poolCounts() -> Counts {
  Counts(allocations: poolAllocations.load(ordering: .relaxed),
         deallocations: poolDeallocations.load(ordering: .relaxed))
}

func otherPoolCounts() -> Counts {
  Counts(allocations: otherPoolAllocations.load(ordering: .relaxed),
         deallocations: otherPoolDeallocations.load(ordering: .relaxed))
}

/// Runs `body` and returns how many allocations and deallocations it made
/// through `Pool` and `OtherPool`.
func counting(_ body: () -> Void) -> (pool: Counts, otherPool: Counts) {
  let pool = poolCounts(), otherPool = otherPoolCounts()
  body()
  let poolAfter = poolCounts(), otherPoolAfter = otherPoolCounts()
  return (Counts(allocations: poolAfter.allocations - pool.allocations,
                 deallocations: poolAfter.deallocations - pool.deallocations),
          Counts(allocations: otherPoolAfter.allocations - otherPool.allocations,
                 deallocations:
                   otherPoolAfter.deallocations - otherPool.deallocations))
}

@allocator(Pool)
class Base {
  var x = 1
  required init() {}
}

class Derived: Base {
  var y = 2
}

@allocator(OtherPool)
final class Overriding: Derived {}

@allocator(Pool)
final class Box<T> {
  var value: T
  init(_ value: T) { self.value = value }
}

final class Plain {}

final class UnownedHolder {
  unowned let target: Base
  init(_ target: Base) { self.target = target }
}

struct InitFailure: Error {}

/// Keeps the optimizer from eliminating an allocation.
@inline(never) @_optimize(none)
func observe(_ object: AnyObject) {}

@allocator(Pool)
final class Failing {
  var x: Int
  init(fail: Bool) throws {
    x = 1
    observe(self)
    if fail { throw InitFailure() }
  }
  init?(failable fail: Bool) {
    x = 2
    observe(self)
    if fail { return nil }
  }
}

@allocator(Pool)
final class Buffer: ManagedBuffer<Int, Int> {}

@allocator(OtherPool)
final class BufferWithoutUsableSize: ManagedBuffer<Int, Int> {}

@allocator(Pool)
final class OverAligned {
  var vector = SIMD8<Double>(repeating: 1)
}

nonisolated(unsafe) var escaped: AnyObject?

@inline(never)
func escape(_ object: AnyObject?) {
  unsafe escaped = object
}

@inline(never)
func makeBase() -> Base { Base() }
@inline(never)
func makeDerived() -> Base { Derived() }
@inline(never)
func makeOverriding() -> Base { Overriding() }
@inline(never)
func make(_ type: Base.Type) -> Base { type.init() }

var tests = TestSuite("ClassAllocator")

tests.test("allocation and deallocation") {
  expectEqual(counting { escape(makeBase()); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectTrue(unsafe lastAllocatedType == Base.self)
  expectTrue(unsafe lastDeallocatedType == Base.self)
  // The object header, then `x`.
  expectEqual(unsafe lastByteCount, 3 * MemoryLayout<Int>.size)
}

tests.test("subclasses") {
  expectEqual(counting { escape(makeDerived()); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectTrue(unsafe lastAllocatedType == Derived.self)

  let counts = counting { escape(makeOverriding()); escape(nil) }
  expectEqual(counts.pool, Counts(allocations: 0, deallocations: 0))
  expectEqual(counts.otherPool, Counts(allocations: 1, deallocations: 1))
}

tests.test("dynamic allocation through a metatype") {
  expectEqual(counting { escape(make(Derived.self)); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(make(Overriding.self)); escape(nil) }.otherPool,
              Counts(allocations: 1, deallocations: 1))
}

tests.test("generic classes") {
  expectEqual(counting { escape(Box(1)); escape(Box("two")); escape(nil) }.pool,
              Counts(allocations: 2, deallocations: 2))
  expectTrue(unsafe lastDeallocatedType == Box<String>.self)
}

tests.test("classes without an allocator") {
  let counts = counting { escape(Plain()); escape(nil) }
  expectEqual(counts.pool, Counts(allocations: 0, deallocations: 0))
  expectEqual(counts.otherPool, Counts(allocations: 0, deallocations: 0))
}

tests.test("unowned references defer the free") {
  var holder: UnownedHolder?
  var object: Base? = makeDerived()
  let before = poolCounts()
  holder = UnownedHolder(object!)
  escape(holder)
  object = nil
  // Deinitialized, but the memory stays allocated for the unowned reference.
  expectEqual(poolCounts().deallocations, before.deallocations)
  holder = nil
  escape(nil)
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  _ = holder
  _ = object
}

tests.test("weak references") {
  let counts = counting {
    var object: Base? = makeBase()
    weak var weakObject = object
    escape(object)
    expectNotNil(weakObject)
    object = nil
    escape(nil)
    expectNil(weakObject)
    _ = object
  }
  expectEqual(counts.pool, Counts(allocations: 1, deallocations: 1))
}

tests.test("failing initializers") {
  expectEqual(counting { _ = try? Failing(fail: true) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(Failing(failable: true)); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
}

tests.test("ManagedBuffer") {
  var capacity = 0
  let counts = counting {
    let buffer = Buffer.create(minimumCapacity: 10) { _ in 0 }
    capacity = buffer.capacity
    escape(buffer)
    escape(nil)
  }
  expectEqual(counts.pool, Counts(allocations: 1, deallocations: 1))
  expectGE(capacity, 10)
  expectGT(usableSizeCalls.load(ordering: .relaxed), 0)
}

tests.test("ManagedBuffer capacity without usableSize")
  .crashOutputMatches("does not implement usableSize(of:for:)")
  .code {
  let buffer = BufferWithoutUsableSize.create(minimumCapacity: 10) { _ in 0 }
  expectCrashLater()
  _ = buffer.capacity
}

tests.test("alignment") {
  expectEqual(counting { escape(OverAligned()); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectEqual(unsafe lastAlignment, MemoryLayout<SIMD8<Double>>.alignment)
}

tests.test("freed on another thread") {
  let counts = counting {
    let objects = (0..<1000).map { _ in makeDerived() }
    let thread = Thread { _ = objects }
    thread.join()
  }
  expectEqual(counts.pool, Counts(allocations: 1000, deallocations: 1000))
}

tests.test("concurrent first use") {
  let counts = counting {
    let threads = (0..<8).map { _ in
      Thread {
        for _ in 0..<10_000 {
          escapeLocal(makeOverriding())
        }
      }
    }
    for thread in threads { thread.join() }
  }
  expectEqual(counts.otherPool,
              Counts(allocations: 80_000, deallocations: 80_000))
}

@inline(never)
func escapeLocal(_ object: AnyObject) {
  _ = Unmanaged.passUnretained(object)
}

/// A minimal pthread wrapper.
final class Thread {
  #if canImport(Darwin)
  private var thread: pthread_t?
  #else
  private var thread = pthread_t()
  #endif

  init(_ body: @escaping () -> Void) {
    final class Context {
      let body: () -> Void
      init(_ body: @escaping () -> Void) { self.body = body }
    }
    let context = Unmanaged.passRetained(Context(body)).toOpaque()
    let result = unsafe pthread_create(&thread, nil, { context in
      #if canImport(Darwin)
      unsafe Unmanaged<Context>.fromOpaque(context).takeRetainedValue().body()
      #else
      unsafe Unmanaged<Context>.fromOpaque(context!).takeRetainedValue().body()
      #endif
      return nil
    }, context)
    precondition(result == 0)
  }

  func join() {
    #if canImport(Darwin)
    pthread_join(thread!, nil)
    #else
    pthread_join(thread, nil)
    #endif
  }
}

runAllTests()
