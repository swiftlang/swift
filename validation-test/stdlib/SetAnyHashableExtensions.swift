// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

class TestHashableBase : Hashable {
  var value: Int
  var identity: Int
  init(_ value: Int, identity: Int) {
    self.value = value
    self.identity = identity
  }
  var hashValue: Int {
    return value
  }
  static func == (
    lhs: TestHashableBase,
    rhs: TestHashableBase
  ) -> Bool {
    return lhs.value == rhs.value
  }
}

class TestHashableDerivedA : TestHashableBase {}
class TestHashableDerivedB : TestHashableBase {}

var SetTests = TestSuite("Set")

SetTests.test("contains<Hashable>(_:)") {
  let s: Set<AnyHashable> = [
    AnyHashable(1010 as UInt16), AnyHashable(2020), AnyHashable(3030.0)
  ]
  for i in [1010, 2020, 3030] {
    // We must be able to look up the same number in any representation.
    expectTrue(s.contains(UInt16(i)))
    expectTrue(s.contains(UInt32(i)))
    expectTrue(s.contains(UInt64(i)))
    expectTrue(s.contains(UInt(i)))
    expectTrue(s.contains(Int16(i)))
    expectTrue(s.contains(Int32(i)))
    expectTrue(s.contains(Int64(i)))
    expectTrue(s.contains(Int(i)))
    expectTrue(s.contains(Float(i)))
    expectTrue(s.contains(Double(i)))

    expectFalse(s.contains(String(i)))
  }
}

SetTests.test("index<Hashable>(of:)") {
  let a = AnyHashable(1010 as UInt16)
  let b = AnyHashable(2020)
  let c = AnyHashable(3030.0)
  let s: Set<AnyHashable> = [a, b, c]
  for (element, i) in [(a, 1010), (b, 2020), (c, 3030)] {
    let index = s.firstIndex(of: element)!

    // We must be able to look up the same number in any representation.
    expectEqual(index, s.firstIndex(of: UInt16(i)))
    expectEqual(index, s.firstIndex(of: UInt32(i)))
    expectEqual(index, s.firstIndex(of: UInt64(i)))
    expectEqual(index, s.firstIndex(of: UInt(i)))
    expectEqual(index, s.firstIndex(of: Int16(i)))
    expectEqual(index, s.firstIndex(of: Int32(i)))
    expectEqual(index, s.firstIndex(of: Int64(i)))
    expectEqual(index, s.firstIndex(of: Int(i)))
    expectEqual(index, s.firstIndex(of: Float(i)))
    expectEqual(index, s.firstIndex(of: Double(i)))
  }
}

SetTests.test("insert<Hashable>(_:)") {
  var s: Set<AnyHashable> = [
    AnyHashable(MinimalHashableValue(1010, identity: 1)),
    AnyHashable(MinimalHashableValue(2020, identity: 1)),
    AnyHashable(MinimalHashableClass(3030, identity: 1)),
  ]

  do {
    let (inserted, memberAfterInsert) =
      s.insert(MinimalHashableValue(1010, identity: 2))
    expectFalse(inserted)
    expectEqual(1, memberAfterInsert.identity)
  }
  do {
    let (inserted, memberAfterInsert) =
      s.insert(MinimalHashableValue(2020, identity: 2))
    expectFalse(inserted)
    expectEqual(1, memberAfterInsert.identity)
  }
  do {
    let (inserted, memberAfterInsert) =
      s.insert(MinimalHashableClass(3030, identity: 2))
    expectFalse(inserted)
    expectEqual(1, memberAfterInsert.identity)
  }

  do {
    let (inserted, memberAfterInsert) =
      s.insert(MinimalHashableClass(1010, identity: 2))
    expectTrue(inserted)
    expectEqual(2, memberAfterInsert.identity)
  }
  do {
    let (inserted, memberAfterInsert) =
      s.insert(MinimalHashableClass(2020, identity: 2))
    expectTrue(inserted)
    expectEqual(2, memberAfterInsert.identity)
  }
  do {
    let (inserted, memberAfterInsert) =
      s.insert(MinimalHashableValue(3030, identity: 2))
    expectTrue(inserted)
    expectEqual(2, memberAfterInsert.identity)
  }

  let expected: Set<AnyHashable> = [
    AnyHashable(MinimalHashableValue(1010, identity: 1)),
    AnyHashable(MinimalHashableValue(2020, identity: 1)),
    AnyHashable(MinimalHashableClass(3030, identity: 1)),

    AnyHashable(MinimalHashableClass(1010, identity: 2)),
    AnyHashable(MinimalHashableClass(2020, identity: 2)),
    AnyHashable(MinimalHashableValue(3030, identity: 2)),
  ]
  expectEqual(expected, s)
}

SetTests.test("insert<Hashable>(_:)/CastTrap")
  .crashOutputMatches("Could not cast value of type 'main.TestHashableDerivedA'")
  .crashOutputMatches("to 'main.TestHashableDerivedB'")
  .code {
  var s: Set<AnyHashable> = [
    AnyHashable(TestHashableDerivedA(1010, identity: 1)),
  ]

  do {
    let (inserted, memberAfterInsert) =
      s.insert(TestHashableDerivedA(1010, identity: 2))
    expectFalse(inserted)
    expectEqual(1, memberAfterInsert.identity)
  }

  expectCrashLater()
  _ = s.insert(TestHashableDerivedB(1010, identity: 3))
}

SetTests.test("update<Hashable>(with:)") {
  var s: Set<AnyHashable> = [
    AnyHashable(MinimalHashableValue(1010, identity: 1)),
    AnyHashable(MinimalHashableValue(2020, identity: 1)),
    AnyHashable(MinimalHashableClass(3030, identity: 1)),
  ]

  do {
    let old = s.update(with: MinimalHashableValue(1010, identity: 2))!
    expectEqual(1, old.identity)
  }
  do {
    let old = s.update(with: MinimalHashableValue(2020, identity: 2))!
    expectEqual(1, old.identity)
  }
  do {
    let old = s.update(with: MinimalHashableClass(3030, identity: 2))!
    expectEqual(1, old.identity)
  }

  expectNil(s.update(with: MinimalHashableClass(1010, identity: 2)))
  expectNil(s.update(with: MinimalHashableClass(2020, identity: 2)))
  expectNil(s.update(with: MinimalHashableValue(3030, identity: 2)))

  let expected: Set<AnyHashable> = [
    AnyHashable(MinimalHashableValue(1010, identity: 2)),
    AnyHashable(MinimalHashableValue(2020, identity: 2)),
    AnyHashable(MinimalHashableClass(3030, identity: 2)),

    AnyHashable(MinimalHashableClass(1010, identity: 2)),
    AnyHashable(MinimalHashableClass(2020, identity: 2)),
    AnyHashable(MinimalHashableValue(3030, identity: 2)),
  ]
  expectEqual(expected, s)
}

SetTests.test("update<Hashable>(with:)/CastTrap")
  .crashOutputMatches("Could not cast value of type 'main.TestHashableDerivedA'")
  .crashOutputMatches("to 'main.TestHashableDerivedB'")
  .code {
  var s: Set<AnyHashable> = [
    AnyHashable(TestHashableDerivedA(1010, identity: 1)),
  ]

  do {
    let old = s.update(with: TestHashableDerivedA(1010, identity: 2))!
    expectEqual(1, old.identity)
  }

  expectCrashLater()
  s.update(with: TestHashableDerivedB(1010, identity: 3))
}

SetTests.test("remove<Hashable>(_:)") {
  var s: Set<AnyHashable> = [
    AnyHashable(MinimalHashableValue(1010, identity: 1)),
    AnyHashable(MinimalHashableValue(2020, identity: 1)),
    AnyHashable(MinimalHashableClass(3030, identity: 1)),
  ]

  expectNil(s.remove(MinimalHashableClass(1010)))
  expectNil(s.remove(MinimalHashableClass(2020)))
  expectNil(s.remove(MinimalHashableValue(3030)))

  expectEqual(3, s.count)

  do {
    let old = s.remove(MinimalHashableValue(1010, identity: 2))!
    expectEqual(1010, old.value)
    expectEqual(1, old.identity)
  }
  do {
    let old = s.remove(MinimalHashableValue(2020, identity: 2))!
    expectEqual(2020, old.value)
    expectEqual(1, old.identity)
  }
  do {
    let old = s.remove(MinimalHashableClass(3030, identity: 2))!
    expectEqual(3030, old.value)
    expectEqual(1, old.identity)
  }
}

SetTests.test("remove<Hashable>(_:)/CastTrap")
  .crashOutputMatches("Could not cast value of type 'main.TestHashableDerivedA'")
  .crashOutputMatches("to 'main.TestHashableDerivedB'")
  .code {
  var s: Set<AnyHashable> = [
    AnyHashable(TestHashableDerivedA(1010, identity: 1)),
    AnyHashable(TestHashableDerivedA(2020, identity: 1)),
  ]

  do {
    let old = s.remove(TestHashableDerivedA(1010, identity: 2))!
    expectEqual(1010, old.value)
    expectEqual(1, old.identity)
  }

  expectCrashLater()
  s.remove(TestHashableDerivedB(2020, identity: 2))
}

SetTests.test("Hashable/Conversions") {
  let input: [Set<AnyHashable>] = [
    [10 as UInt8, 20 as UInt8, 30 as UInt8],
    [10 as UInt16, 20 as UInt16, 30 as UInt16],
    [10 as UInt32, 20 as UInt32, 30 as UInt32],
    [10 as UInt64, 20 as UInt64, 30 as UInt64],
    [10 as UInt, 20 as UInt, 30 as UInt],
    [10 as Int8, 20 as Int8, 30 as Int8],
    [10 as Int16, 20 as Int16, 30 as Int16],
    [10 as Int32, 20 as Int32, 30 as Int32],
    [10 as Int64, 20 as Int64, 30 as Int64],
    [10 as Int, 20 as Int, 30 as Int],
    [10 as Float, 20 as Float, 30 as Float],
    [10 as Double, 20 as Double, 30 as Double],
    [[1, 2, 3] as Set<Int>, [2, 3, 4] as Set<UInt8>, [3, 4, 5] as Set<Float>],
    [[1, 2, 3] as Set<Int8>, [2, 3, 4] as Set<Double>, [3, 4, 5] as Set<Int32>],
    [[1, 2, 3] as Set<UInt32>, [2, 3, 4] as Set<Int16>, [3, 4, 5] as Set<UInt>],
  ]

  checkHashable(input, equalityOracle: { ($0 < 12) == ($1 < 12) })
}


runAllTests()

