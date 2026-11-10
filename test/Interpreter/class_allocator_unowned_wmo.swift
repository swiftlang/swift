// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -target %target-future-triple -wmo -Onone -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-enable-experimental-feature ClassAllocators -target %target-future-triple -wmo -O -Xfrontend -sil-verify-all)

// The last unowned release calls the allocator directly, though Base isn't
// final.
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple -wmo -Onone -module-name main -emit-ir %s | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_ClassAllocators
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s4main13UnownedHolderCfd"(
// CHECK: [[ALLOCATION:%.*]] = call ptr @swift_unownedReleaseBegin(ptr [[OBJECT:%.*]])
// CHECK: unowned.free:
// CHECK: [[METADATA:%.*]] = load ptr, ptr [[OBJECT]]
// CHECK: call swiftcc void @"$s4main4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr [[ALLOCATION]], i64 {{%.*}}, i64 {{%.*}}, ptr [[METADATA]])
// CHECK: }

struct Allocation {
  var pointer: UnsafeMutableRawPointer
  var byteCount: Int
  var alignment: Int
  var type: ObjectIdentifier
}

nonisolated(unsafe) var allocations: [Allocation] = []
nonisolated(unsafe) var deallocations: [Allocation] = []
nonisolated(unsafe) var deinits = 0

enum Pool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    let pointer = UnsafeMutableRawPointer.allocate(
      byteCount: byteCount, alignment: alignment)
    unsafe allocations.append(Allocation(
      pointer: pointer, byteCount: byteCount, alignment: alignment,
      type: ObjectIdentifier(type)))
    return unsafe pointer
  }

  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe deallocations.append(Allocation(
      pointer: pointer, byteCount: byteCount, alignment: alignment,
      type: ObjectIdentifier(type)))
    unsafe pointer.deallocate()
  }
}

@allocator(Pool)
class Base {
  var x = 1
  init() {}
  deinit { deinits += 1 }
}

class Derived: Base {
  var a = 2, b = 3, c = 4
}

class OverAlignedDerived: Base {
  var vector = SIMD8<Double>(repeating: 1)
}

class GenericDerived<T>: Derived {
  var value: T
  init(_ value: T) {
    self.value = value
    super.init()
  }
}

final class UnownedHolder {
  unowned let target: Base
  init(_ target: Base) { self.target = target }
}

@inline(never)
func make(_ index: Int) -> Base {
  switch index {
  case 0: return Base()
  case 1: return Derived()
  case 2: return OverAlignedDerived()
  default: return GenericDerived<(Int, Int, Int)>((5, 6, 7))
  }
}

nonisolated(unsafe) var escaped: AnyObject?

@inline(never)
func escape(_ object: AnyObject?) {
  escaped = object
}

var tests = TestSuite("ClassAllocatorUnownedWMO")

let expectedTypes: [AnyClass] = [
  Base.self, Derived.self, OverAlignedDerived.self,
  GenericDerived<(Int, Int, Int)>.self,
]

for (index, expectedType) in expectedTypes.enumerated() {
  tests.test("unowned Base reference to \(expectedType) outlives the object") {
    allocations = []
    deallocations = []
    deinits = 0
    var object: Base? = make(index)
    var holder: UnownedHolder? = UnownedHolder(object!)
    escape(holder)
    expectEqual(allocations.count, 1)
    let allocation = allocations[0]
    expectEqual(allocation.type, ObjectIdentifier(expectedType))

    object = nil
    // Deinitialized, but the memory stays allocated for the unowned reference.
    expectEqual(deinits, 1)
    expectEqual(deallocations.count, 0)

    holder = nil
    escape(nil)
    // Freed exactly once, with the instance's own size, alignment and class.
    expectEqual(deinits, 1)
    expectEqual(deallocations.count, 1)
    let deallocation = deallocations[0]
    expectEqual(deallocation.pointer, allocation.pointer)
    expectEqual(deallocation.byteCount, allocation.byteCount)
    expectEqual(deallocation.alignment, allocation.alignment)
    expectEqual(deallocation.type, ObjectIdentifier(expectedType))
    _ = object
    _ = holder
  }
}

tests.test("subclass instances are larger") {
  allocations = []
  escape(make(0))
  escape(make(1))
  escape(make(2))
  escape(nil)
  expectLT(allocations[0].byteCount, allocations[1].byteCount)
  expectLT(allocations[1].byteCount, allocations[2].byteCount)
  expectGT(allocations[2].alignment, allocations[0].alignment)
}

runAllTests()
