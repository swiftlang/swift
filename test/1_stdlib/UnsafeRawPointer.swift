// RUN: %target-run-simple-swiftgyb
// REQUIRES: executable_test

import StdlibUnittest

var UnsafeMutableRawPointerExtraTestSuite =
  TestSuite("UnsafeMutableRawPointerExtra")

class Missile {
  static var missilesLaunched = 0
  let number: Int
  init(_ number: Int) { self.number = number }
  deinit { Missile.missilesLaunched += 1 }
}

UnsafeMutableRawPointerExtraTestSuite.test("initializeMemory") {
  Missile.missilesLaunched = 0
  do {
    let sizeInBytes = 3 * strideof(Missile.self)
    var p1 = UnsafeMutableRawPointer.allocate(bytes: sizeInBytes)
    defer {
      p1.deallocate(bytes: sizeInBytes)
    }
    var ptrM = p1.initializeMemory(as: Missile.self, to: Missile(1))
    p1.initializeMemory(as: Missile.self, at: 1, count: 2, to: Missile(2))
    expectEqual(1, ptrM[0].number)
    expectEqual(2, ptrM[1].number)
    expectEqual(2, ptrM[2].number)

    var p2 = UnsafeMutableRawPointer.allocate(bytes: sizeInBytes)
    defer {
      p2.deallocate(bytes: sizeInBytes)
    }
    let ptrM2 = p2.moveInitializeMemory(as: Missile.self, from: ptrM, count: 3)
    defer {
      ptrM2.deinitialize(count: 3)
    }
    // p1 is now deinitialized.
    expectEqual(1, ptrM2[0].number)
    expectEqual(2, ptrM2[1].number)
    expectEqual(2, ptrM2[2].number)

    ptrM = p1.initializeMemory(
      as: Missile.self, from: (1...3).map(Missile.init))
    defer {
      ptrM.deinitialize(count: 3)
    }
    expectEqual(1, ptrM[0].number)
    expectEqual(2, ptrM[1].number)
    expectEqual(3, ptrM[2].number)
  }
  expectEqual(5, Missile.missilesLaunched)
}

UnsafeMutableRawPointerExtraTestSuite.test("load/store") {
  let sizeInBytes = 3 * strideof(Int.self)
  var p1 = UnsafeMutableRawPointer.allocate(bytes: sizeInBytes)
  defer {
    p1.deallocate(bytes: sizeInBytes)
  }
  let ptrI = p1.initializeMemory(as: Int.self, from: 1...3)
  defer {
    ptrI.deinitialize(count: 3)
  }
  expectEqual(1, p1.load(as: Int.self))
  expectEqual(2, p1.load(fromByteOffset: strideof(Int.self), as: Int.self))
  expectEqual(3, p1.load(fromByteOffset: 2*strideof(Int.self), as: Int.self))
  p1.storeBytes(of: 4, as: Int.self)
  p1.storeBytes(of: 5, toByteOffset: strideof(Int.self), as: Int.self)
  p1.storeBytes(of: 6, toByteOffset: 2 * strideof(Int.self), as: Int.self)
  expectEqual(4, p1.load(as: Int.self))
  expectEqual(5, p1.load(fromByteOffset: strideof(Int.self), as: Int.self))
  expectEqual(6, p1.load(fromByteOffset: 2 * strideof(Int.self), as: Int.self))
}

// --------------------------------------------
// Test bulk initialization from UnsafePointer.

enum Check {
  case LeftOverlap
  case RightOverlap
  case Disjoint
}

func checkRawPointerCorrectness(_ check: Check,
  _ f: (UnsafeMutableRawPointer) -> (as: Int.Type, from: UnsafeMutablePointer<Int>, count: Int) -> UnsafeMutablePointer<Int>) {
  let ptr = UnsafeMutablePointer<Int>.allocate(capacity: 4)
  switch check {
  case .RightOverlap:
    ptr.initialize(to: 1)
    (ptr + 1).initialize(to: 2)
    _ = f(UnsafeMutableRawPointer(ptr + 1))(as: Int.self, from: ptr, count: 2)
    expectEqual(1, ptr[1])
    expectEqual(2, ptr[2])
  case .LeftOverlap:
    (ptr + 1).initialize(to: 2)
    (ptr + 2).initialize(to: 3)
    _ = f(UnsafeMutableRawPointer(ptr))(as: Int.self, from: ptr + 1, count: 2)
    expectEqual(2, ptr[0])
    expectEqual(3, ptr[1])
  case .Disjoint:
    (ptr + 2).initialize(to: 2)
    (ptr + 3).initialize(to: 3)
    _ = f(UnsafeMutableRawPointer(ptr))(as: Int.self, from: ptr + 2, count: 2)
    expectEqual(2, ptr[0])
    expectEqual(3, ptr[1])
    // backwards
    let ptr2 = UnsafeMutablePointer<Int>.allocate(capacity: 4)
    ptr2.initialize(to: 0)
    (ptr2 + 1).initialize(to: 1)
    _ = f(UnsafeMutableRawPointer(ptr2 + 2))(as: Int.self, from: ptr2, count: 2)
    expectEqual(0, ptr2[2])
    expectEqual(1, ptr2[3])
  }
}

func checkPtr(
  _ f: ((UnsafeMutableRawPointer)
    -> (as: Int.Type, from: UnsafeMutablePointer<Int>, count: Int)
    -> UnsafeMutablePointer<Int>)
) -> (Check) -> Void {
  return { checkRawPointerCorrectness($0, f) }
}

func checkPtr(
  _ f: ((UnsafeMutableRawPointer)
    -> (as: Int.Type, from: UnsafePointer<Int>, count: Int)
    -> UnsafeMutablePointer<Int>)
) -> (Check) -> Void {
  return {
    checkRawPointerCorrectness($0) { destPtr in 
      return { f(destPtr)(as: $0, from: UnsafeMutablePointer($1), count: $2) }
    }
  }
}

UnsafeMutableRawPointerExtraTestSuite.test("initializeMemory:as:from:count") {
  let check = checkPtr(UnsafeMutableRawPointer.initializeMemory(as:from:count:))
  check(Check.Disjoint)
  // This check relies on _debugPrecondition() so will only trigger in
  // -Onone mode.
  if _isDebugAssertConfiguration() {
    expectCrashLater()
    check(Check.LeftOverlap)
  }
}

UnsafeMutableRawPointerExtraTestSuite.test("initializeMemory:as:from:count.Right") {
  let check = checkPtr(UnsafeMutableRawPointer.initializeMemory(as:from:count:))
  // This check relies on _debugPrecondition() so will only trigger in
  // -Onone mode.
  if _isDebugAssertConfiguration() {
    expectCrashLater()
    check(Check.RightOverlap)
  }
}

UnsafeMutableRawPointerExtraTestSuite.test("moveInitialize:from") {
  let check =
    checkPtr(UnsafeMutableRawPointer.moveInitializeMemory(as:from:count:))
  check(Check.LeftOverlap)
  check(Check.Disjoint)
  check(Check.RightOverlap)
}

runAllTests()
