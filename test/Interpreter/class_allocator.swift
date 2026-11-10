// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -Onone -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -O -Xfrontend -sil-verify-all)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: swift_feature_BuiltinModule
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Builtin
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

func checkTagAndDeallocate(
  _ memory: UnsafeMutableRawPointer, _ byteCount: Int, _ tag: Int
) {
  let actualTag = unsafe (memory - 8).load(as: Int.self)
  if actualTag != tag {
    fatalError("freed memory that this allocator did not allocate")
  }
  if unsafe (memory - 24).load(as: Int.self) != byteCount {
    fatalError("freed memory with a different byte count")
  }
  let alignment = unsafe (memory - 16).load(as: Int.self)
  unsafe (memory - 8).storeBytes(of: 0, as: Int.self)
  unsafe (memory - alignment).deallocate()
}

let poolAllocations = Atomic<Int>(0)
let poolDeallocations = Atomic<Int>(0)
let otherPoolAllocations = Atomic<Int>(0)
let otherPoolDeallocations = Atomic<Int>(0)
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
    lastAllocatedType = type
    lastAlignment = alignment
    lastByteCount = byteCount
    return unsafe tagAndAllocate(byteCount, alignment, tag)
  }

  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    poolDeallocations.add(1, ordering: .relaxed)
    lastDeallocatedType = type
    unsafe checkTagAndDeallocate(pointer, byteCount, tag)
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
    unsafe checkTagAndDeallocate(pointer, byteCount, tag)
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

@allocator(SystemClassAllocator)
final class OptedOut: Derived {}

final class InheritsOptOut: Base {}

@allocator(Pool)
final class Leaf {
  var value = 0
}

@allocator(Pool)
final class Box<T> {
  var value: T
  init(_ value: T) { self.value = value }
}

class GenericBase<T> {
  var base: T?
}

// The runtime initializes this class's metadata, which has generic ancestry,
// from a static template.
@allocator(Pool)
final class ConcreteSubclass: GenericBase<Int> {
  var x = 1
}

final class Plain {}

final class UnownedHolder {
  unowned let target: Base
  init(_ target: Base) { self.target = target }
}

final class UnownedLeafHolder {
  unowned let target: Leaf
  unowned var optionalTarget: Leaf?
  init(_ target: Leaf) {
    self.target = target
    self.optionalTarget = target
  }
}

final class UnownedAnyObjectHolder {
  unowned let target: AnyObject
  init(_ target: AnyObject) { self.target = target }
}

struct InitFailure: Error {}

/// Keeps the optimizer from eliminating an allocation.
@inline(never) @_optimize(none)
func observe(_ object: AnyObject) {}

@allocator(Pool)
class Failing {
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

final class FailingSubclass: Failing {
  var y = 3
  init(failInSuperclass: Bool) throws {
    try super.init(fail: failInSuperclass)
  }
}

// No stored properties, as ManagedBufferPointer requires.
@allocator(Pool)
final class EmptyBuffer {}

// Larger than IRGen's stack promotion budget.
@allocator(Pool)
final class Big {
  var storage: InlineArray<512, Int> = .init(repeating: 1)
}

@inline(never)
func sum(_ big: Big) -> Int {
  var total = 0
  for i in big.storage.indices { total &+= big.storage[i] }
  return total
}

@allocator(Pool)
final class OverAligned {
  var vector = SIMD8<Double>(repeating: 1)
}

nonisolated(unsafe) var escaped: AnyObject?

@inline(never)
func escape(_ object: AnyObject?) {
  escaped = object
}

@inline(never)
func makeBase() -> Base { Base() }
@inline(never)
func makeDerived() -> Base { Derived() }
@inline(never)
func makeOverriding() -> Base { Overriding() }
@inline(never)
func makeLeaf() -> Leaf { Leaf() }
@inline(never)
func make(_ type: Base.Type) -> Base { type.init() }

var tests = TestSuite("ClassAllocator")

tests.test("allocation and deallocation") {
  expectEqual(counting { escape(makeBase()); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectTrue(lastAllocatedType == Base.self)
  expectTrue(lastDeallocatedType == Base.self)
  // The object header, then `x`.
  expectEqual(lastByteCount, 3 * MemoryLayout<Int>.size)
}

tests.test("subclasses") {
  expectEqual(counting { escape(makeDerived()); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectTrue(lastAllocatedType == Derived.self)

  let counts = counting { escape(makeOverriding()); escape(nil) }
  expectEqual(counts.pool, Counts(allocations: 0, deallocations: 0))
  expectEqual(counts.otherPool, Counts(allocations: 1, deallocations: 1))
}

tests.test("SystemClassAllocator opts out") {
  let counts = counting {
    escape(OptedOut()); escape(nil)
    escape(InheritsOptOut()); escape(nil)
  }
  expectEqual(counts.pool, Counts(allocations: 1, deallocations: 1))
  expectEqual(counts.otherPool, Counts(allocations: 0, deallocations: 0))
  expectTrue(lastAllocatedType == InheritsOptOut.self)
}

tests.test("dynamic allocation through a metatype") {
  expectEqual(counting { escape(make(Derived.self)); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(make(Overriding.self)); escape(nil) }.otherPool,
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(make(OptedOut.self)); escape(nil) }.pool,
              Counts(allocations: 0, deallocations: 0))
}

tests.test("generic classes") {
  expectEqual(counting { escape(Box(1)); escape(Box("two")); escape(nil) }.pool,
              Counts(allocations: 2, deallocations: 2))
  expectTrue(lastDeallocatedType == Box<String>.self)
}

tests.test("generic ancestry") {
  expectEqual(counting { escape(ConcreteSubclass()); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectTrue(lastDeallocatedType == ConcreteSubclass.self)
  // The runtime keeps the template's value witness table.
  expectNotEqual(classFlags(ConcreteSubclass.self) & hasClassAllocator, 0)
  expectTrue(valueWitnessTable(ConcreteSubclass.self) ==
             valueWitnessTable(Base.self))
}

tests.test("classes without an allocator") {
  let counts = counting { escape(Plain()); escape(nil) }
  expectEqual(counts.pool, Counts(allocations: 0, deallocations: 0))
  expectEqual(counts.otherPool, Counts(allocations: 0, deallocations: 0))
}

tests.test("unowned references defer the free: non-final class") {
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
  // The runtime frees it, through the class's value witness table.
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  expectTrue(lastDeallocatedType == Derived.self)
  _ = holder
  _ = object
}

tests.test("unowned references defer the free: final class") {
  var holder: UnownedLeafHolder?
  var object: Leaf? = makeLeaf()
  let before = poolCounts()
  holder = UnownedLeafHolder(object!)
  escape(holder)
  object = nil
  expectEqual(poolCounts().deallocations, before.deallocations)
  holder = nil
  escape(nil)
  // Compiled code frees it, calling the allocator directly.
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  expectTrue(lastDeallocatedType == Leaf.self)
  _ = holder
  _ = object
}

tests.test("unowned references defer the free: AnyObject") {
  var holder: UnownedAnyObjectHolder?
  var object: Base? = makeBase()
  let before = poolCounts()
  holder = UnownedAnyObjectHolder(object!)
  escape(holder)
  object = nil
  expectEqual(poolCounts().deallocations, before.deallocations)
  holder = nil
  escape(nil)
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  _ = holder
  _ = object
}

tests.test("unowned captures") {
  let before = poolCounts()
  var closure: (() -> Int)?
  do {
    let leaf = makeLeaf()
    leaf.value = 42
    closure = { [unowned leaf] in leaf.value }
    expectEqual(closure!(), 42)
  }
  expectEqual(poolCounts().deallocations, before.deallocations)
  closure = nil
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  _ = closure
}

// Weak references to instances of classes with a class allocator use side
// tables from the runtime's allocator, never from the class's allocator.
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

tests.test("weak references: final class") {
  let counts = counting {
    var object: Leaf? = makeLeaf()
    weak var weakObject = object
    weak var otherWeakObject = object
    escape(object)
    expectTrue(weakObject === otherWeakObject)
    object = nil
    escape(nil)
    expectNil(weakObject)
    expectNil(otherWeakObject)
    _ = object
  }
  expectEqual(counts.pool, Counts(allocations: 1, deallocations: 1))
}

tests.test("weak references: the side table outlives the object") {
  var object: Base? = makeDerived()
  weak var weakObject = object
  let before = poolCounts()
  object = nil
  // The instance is freed, but not its side table.
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  expectNil(weakObject)
  // Reuse the instance's memory.
  for _ in 0..<10 { escape(makeDerived()) }
  escape(nil)
  expectNil(weakObject)
  weakObject = nil
  expectEqual(poolCounts().deallocations, before.deallocations + 11)
  _ = object
  _ = weakObject
}

// With a side table, the unowned reference count is in the side table; the
// last unowned release still frees the instance through its allocator.
tests.test("weak and unowned references: non-final class") {
  var holder: UnownedHolder?
  var object: Base? = makeDerived()
  weak var weakObject = object
  let before = poolCounts()
  holder = UnownedHolder(object!)
  escape(holder)
  object = nil
  expectNil(weakObject)
  expectEqual(poolCounts().deallocations, before.deallocations)
  holder = nil
  escape(nil)
  // The runtime frees it, through the class's value witness table.
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  expectTrue(lastDeallocatedType == Derived.self)
  expectNil(weakObject)
  weakObject = nil
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  _ = holder
  _ = object
  _ = weakObject
}

tests.test("weak and unowned references: final class") {
  var holder: UnownedLeafHolder?
  var object: Leaf? = makeLeaf()
  weak var weakObject = object
  let before = poolCounts()
  holder = UnownedLeafHolder(object!)
  escape(holder)
  object = nil
  expectNil(weakObject)
  expectEqual(poolCounts().deallocations, before.deallocations)
  holder = nil
  escape(nil)
  // Compiled code frees it, calling the allocator directly.
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  expectTrue(lastDeallocatedType == Leaf.self)
  expectNil(weakObject)
  _ = holder
  _ = object
  _ = weakObject
}

tests.test("weak and unowned references: the weak reference goes first") {
  var holder: UnownedHolder?
  var object: Base? = makeDerived()
  let before = poolCounts()
  do {
    weak var weakObject = object
    expectNotNil(weakObject)
    _ = weakObject
  }
  holder = UnownedHolder(object!)
  escape(holder)
  object = nil
  expectEqual(poolCounts().deallocations, before.deallocations)
  holder = nil
  escape(nil)
  expectEqual(poolCounts().deallocations, before.deallocations + 1)
  _ = holder
  _ = object
}

tests.test("failing initializers") {
  expectEqual(counting { _ = try? Failing(fail: true) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(Failing(failable: true)); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  // The superclass initializer fails; the object is a subclass instance.
  expectEqual(counting { _ = try? FailingSubclass(failInSuperclass: true) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectTrue(lastDeallocatedType == FailingSubclass.self)
  expectEqual(counting {
                escape(try? FailingSubclass(failInSuperclass: false))
                escape(nil)
              }.pool,
              Counts(allocations: 1, deallocations: 1))
}

tests.test("objects that don't escape") {
  // At -O, these are stack-promoted, but they're larger than IRGen's stack
  // promotion budget, so IRGen allocates them with the allocator.
  let counts = counting {
    for _ in 0..<3 {
      let a = Big(), b = Big(), c = Big()
      expectEqual(sum(a) + sum(b) + sum(c), 3 * 512)
    }
  }
  expectEqual(counts.pool, Counts(allocations: 9, deallocations: 9))
}

tests.test("alignment") {
  expectEqual(counting { escape(OverAligned()); escape(nil) }.pool,
              Counts(allocations: 1, deallocations: 1))
  expectEqual(lastAlignment, MemoryLayout<SIMD8<Double>>.alignment)
}

/// The value witness table of `type`.
func valueWitnessTable(_ type: Any.Type) -> UnsafeRawPointer {
  let metadata = unsafe unsafeBitCast(type, to: UnsafeRawPointer.self)
  return unsafe (metadata - MemoryLayout<UnsafeRawPointer>.size)
    .load(as: UnsafeRawPointer.self)
}

/// The flags of the value witness table of `type`.
func valueWitnessFlags(_ type: Any.Type) -> UInt32 {
  // The flags follow eight witnesses, the size and the stride.
  return unsafe valueWitnessTable(type).load(
    fromByteOffset: 10 * MemoryLayout<UnsafeRawPointer>.size, as: UInt32.self)
}

/// The class allocator witness of `type`'s value witness table, which follows
/// the base table: the flags and the extra inhabitant count.
func classAllocatorWitness(_ type: AnyClass) -> UnsafeRawPointer? {
  return unsafe valueWitnessTable(type).load(
    fromByteOffset: 10 * MemoryLayout<UnsafeRawPointer>.size + 8,
    as: UnsafeRawPointer?.self)
}

/// The class flags of `type`'s metadata.
func classFlags(_ type: AnyClass) -> UInt32 {
  #if canImport(ObjectiveC)
  let offset = 5
  #else
  let offset = 2
  #endif
  let metadata = unsafe unsafeBitCast(type, to: UnsafeRawPointer.self)
  return unsafe metadata.load(
    fromByteOffset: offset * MemoryLayout<UnsafeRawPointer>.size,
    as: UInt32.self)
}

/// Builtin.FixedArray's metadata is instantiated from its element's.
@inline(never)
func fixedArrayType<T>(_: T.Type) -> Any.Type {
  return Builtin.FixedArray<3, T>.self
}

let hasClassAllocator: UInt32 = 0x20
let hasEnumWitnesses: UInt32 = 0x0020_0000

tests.test("value witness tables") {
  // Classes with a class allocator have the class flag, which is the only
  // marker of their value witness table's class allocator witness.
  let withAllocator: [AnyClass] = [
    Base.self, Derived.self, Overriding.self, InheritsOptOut.self, Leaf.self,
    Box<Int>.self, Box<String>.self, ConcreteSubclass.self,
  ]
  for type in withAllocator {
    expectNotEqual(classFlags(type) & hasClassAllocator, 0, "\(type)")
    expectNotNil(classAllocatorWitness(type), "\(type)")
    // The base table is Builtin.NativeObject's, with the same flags.
    expectEqual(valueWitnessFlags(type), valueWitnessFlags(Plain.self),
                "\(type)")
  }
  for type: AnyClass in [Plain.self, OptedOut.self] {
    expectEqual(classFlags(type) & hasClassAllocator, 0, "\(type)")
  }

  // Classes with the same allocator share a table, whether their metadata
  // is static, instantiated or initialized from a template.
  let poolTable = valueWitnessTable(Base.self)
  for type: AnyClass in [Derived.self, InheritsOptOut.self, Leaf.self,
                         Box<Int>.self, Box<String>.self,
                         ConcreteSubclass.self] {
    expectTrue(valueWitnessTable(type) == poolTable, "\(type)")
  }
  expectTrue(valueWitnessTable(Overriding.self) != poolTable)
  expectTrue(valueWitnessTable(Plain.self) != poolTable)

  // Tables derived from a class's table are the same as those derived from
  // the table of a class without an allocator.
  let optionalFlags = valueWitnessFlags(Optional<Leaf>.self)
  expectEqual(optionalFlags, valueWitnessFlags(Optional<Plain>.self))
  expectNotEqual(optionalFlags & hasEnumWitnesses, 0)
  expectEqual(valueWitnessFlags(InlineArray<3, Leaf>.self),
              valueWitnessFlags(InlineArray<3, Plain>.self))
  expectEqual(valueWitnessFlags(fixedArrayType(Leaf.self)),
              valueWitnessFlags(fixedArrayType(Plain.self)))

  // Generic code over the derived types works.
  func roundTrip<T>(_ value: T) -> T { value }
  let counts = counting {
    var optional: Leaf? = Leaf()
    optional = roundTrip(optional)
    escape(optional)
    var array: InlineArray<2, Leaf?> = [Leaf(), nil]
    array = roundTrip(array)
    escape(array[0])
    escape(nil)
  }
  expectEqual(counts.pool, Counts(allocations: 2, deallocations: 2))
}

tests.test("freed on another thread") {
  let counts = counting {
    let objects = (0..<1000).map { _ in makeDerived() }
    let thread = Thread { _ = objects }
    thread.join()
  }
  expectEqual(counts.pool, Counts(allocations: 1000, deallocations: 1000))
}

tests.test("concurrent allocation") {
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
func makeBufferPointer(_ bufferClass: AnyClass) {
  _ = ManagedBufferPointer<Int, Int>(
    bufferClass: bufferClass, minimumCapacity: 1) { _, _ in 0 }
}

tests.test("ManagedBufferPointer traps")
  .crashOutputMatches("ManagedBufferPointer cannot allocate an instance of class")
  .code {
  expectCrashLater()
  makeBufferPointer(EmptyBuffer.self)
}

#if canImport(ObjectiveC)
import ObjectiveC

tests.test("+allocWithZone: traps")
  .crashOutputMatches("+allocWithZone: cannot allocate an instance of class")
  .code {
  typealias AllocFunction = @convention(c) (AnyClass, Selector) -> UnsafeRawPointer?
  let msgSend = unsafe unsafeBitCast(
    dlsym(UnsafeMutableRawPointer(bitPattern: -2), "objc_msgSend"),
    to: AllocFunction.self)
  expectCrashLater()
  _ = unsafe msgSend(Leaf.self, sel_registerName("alloc"))
}
#endif

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
