// Stack-promoted instances of classes with class allocators never reach the
// allocator; if IRGen can't put one on the stack, it allocates and frees it
// with the allocator.

// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -Onone -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -O -Xfrontend -sil-verify-all -D STACK)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -O -Xfrontend -sil-verify-all -Xfrontend -stack-promotion-limit -Xfrontend 0)

// Releases that aren't devirtualized.
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -O -Xfrontend -sil-verify-all -Xllvm -sil-disable-pass=release-devirtualizer -D STACK)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -enable-experimental-feature BuiltinModule -target %target-future-triple -O -Xfrontend -sil-verify-all -Xllvm -sil-disable-pass=release-devirtualizer -Xfrontend -stack-promotion-limit -Xfrontend 0)

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

struct Counts: Equatable, CustomStringConvertible {
  var allocations: Int
  var deallocations: Int
  var description: String { "\(allocations)/\(deallocations)" }
}

// A malloc-backed allocator that tags its memory, so that freeing memory it
// did not allocate, or freeing a stack object, is detected.
let headerSize = 32
let poolAllocations = Atomic<Int>(0)
let poolDeallocations = Atomic<Int>(0)

enum Pool: @unsafe ClassAllocator {
  static let tag = 0x506f6f6c

  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    poolAllocations.add(1, ordering: .relaxed)
    let alignment = max(alignment, headerSize)
    let base = UnsafeMutableRawPointer.allocate(
      byteCount: byteCount + alignment, alignment: alignment)
    let memory = unsafe base + alignment
    unsafe (memory - 8).storeBytes(of: tag, as: Int.self)
    unsafe (memory - 16).storeBytes(of: alignment, as: Int.self)
    unsafe (memory - 24).storeBytes(of: byteCount, as: Int.self)
    return unsafe memory
  }

  static func deallocate(
    _ memory: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    poolDeallocations.add(1, ordering: .relaxed)
    if unsafe (memory - 8).load(as: Int.self) != tag {
      fatalError("freed memory that this allocator did not allocate")
    }
    if unsafe (memory - 24).load(as: Int.self) != byteCount {
      fatalError("freed memory with a different byte count")
    }
    let alignment = unsafe (memory - 16).load(as: Int.self)
    unsafe (memory - 8).storeBytes(of: 0, as: Int.self)
    unsafe (memory - alignment).deallocate()
  }
}

/// Runs `body` and returns how many allocations and deallocations it made
/// through `Pool`.
func counting(_ body: () -> Void) -> Counts {
  let allocations = poolAllocations.load(ordering: .relaxed)
  let deallocations = poolDeallocations.load(ordering: .relaxed)
  body()
  return Counts(
    allocations: poolAllocations.load(ordering: .relaxed) - allocations,
    deallocations: poolDeallocations.load(ordering: .relaxed) - deallocations)
}

/// The counts of `n` objects of a stack-promoted allocation: none if IRGen
/// puts them on the stack, otherwise one allocation and one deallocation each.
func stackPromoted(_ n: Int) -> Counts {
  #if STACK
  return Counts(allocations: 0, deallocations: 0)
  #else
  return Counts(allocations: n, deallocations: n)
  #endif
}

func isOptimized() -> Bool { !_isDebugAssertConfiguration() }

@allocator(Pool)
final class Node {
  var value: Int
  init(_ value: Int) { self.value = value }
}

@inline(never)
func read(_ node: Node) -> Int { node.value }

nonisolated(unsafe) var deinitCount = 0

final class Payload {
  var value: Int
  init(_ value: Int) { self.value = value }
}

// A deinit that isn't trivial, and a stored reference that it releases.
@allocator(Pool)
final class Logged {
  var payload: Payload
  init(_ payload: Payload) { self.payload = payload }
  deinit { deinitCount += payload.value }
}

@inline(never)
func read(_ logged: Logged) -> Int { logged.payload.value }

@allocator(Pool)
final class Box<T> {
  var value: T
  @inline(__always)
  init(_ value: T) { self.value = value }
}

@inline(never)
func read<T>(_ box: Box<T>) -> T { box.value }

// Box<T>'s layout isn't fixed in unspecialized code, so IRGen allocates it
// with the allocator even if it's stack-promoted.
@_semantics("optimize.sil.specialize.generic.never")
@inline(never)
func genericLocal<T>(_ value: T) -> T {
  let box = Box(value)
  return read(box)
}

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
final class Table {
  var a: Int, b: Int
  init(a: Int, b: Int) { self.a = a; self.b = b }
}

@inline(never)
func read(_ table: Table, _ i: Int) -> Int { i == 0 ? table.a : table.b }

// With optimization, the object is outlined into a statically initialized
// object.
@inline(never)
func lookup(_ i: Int) -> Int {
  var table = Table(a: 10, b: 20)
  Builtin.endCOWMutation(&table)
  return read(table, i)
}

nonisolated(unsafe) var escaped: AnyObject?

@inline(never)
func escape(_ object: AnyObject?) { escaped = object }

var tests = TestSuite("ClassAllocatorStackPromotion")

tests.test("stack-promoted locals") {
  var total = 0
  let counts = counting {
    for i in 0..<10 {
      let node = Node(i)
      total &+= read(node)
    }
  }
  expectEqual(total, 45)
  expectEqual(counts, isOptimized() ? stackPromoted(10)
                                    : Counts(allocations: 10, deallocations: 10))
}

tests.test("stack-promoted locals with a deinit") {
  deinitCount = 0
  var payload = Payload(1)
  var total = 0
  let counts = counting {
    for _ in 0..<5 {
      let logged = Logged(payload)
      total &+= read(logged)
    }
  }
  expectEqual(total, 5)
  expectEqual(deinitCount, 5)
  expectTrue(isKnownUniquelyReferenced(&payload))
  expectEqual(counts, isOptimized() ? stackPromoted(5)
                                    : Counts(allocations: 5, deallocations: 5))
}

tests.test("layout that isn't fixed") {
  var total = 0
  let counts = counting {
    for i in 0..<4 { total &+= genericLocal(i) }
    expectEqual(genericLocal("a"), "a")
  }
  expectEqual(total, 6)
  expectEqual(counts, Counts(allocations: 5, deallocations: 5))
}

tests.test("larger than the stack budget") {
  let counts = counting {
    for _ in 0..<3 {
      let a = Big(), b = Big()
      expectEqual(sum(a) + sum(b), 2 * 512)
    }
  }
  expectEqual(counts, Counts(allocations: 6, deallocations: 6))
}

tests.test("outlined objects") {
  var total = 0
  let counts = counting {
    for i in 0..<3 { total &+= lookup(i) }
  }
  expectEqual(total, 50)
  expectEqual(counts, isOptimized() ? Counts(allocations: 0, deallocations: 0)
                                    : Counts(allocations: 3, deallocations: 3))
}

tests.test("objects that escape") {
  let counts = counting {
    escape(Node(1))
    escape(nil)
  }
  expectEqual(counts, Counts(allocations: 1, deallocations: 1))
}

runAllTests()
