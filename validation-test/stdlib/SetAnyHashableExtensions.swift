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
    AnyHashable(1010), AnyHashable(2020), AnyHashable(3030.0)
  ]
  expectTrue(s.contains(1010))
  expectTrue(s.contains(2020))
  expectTrue(s.contains(3030.0))

  expectFalse(s.contains(1010.0))
  expectFalse(s.contains(2020.0))
  expectFalse(s.contains(3030))
}

SetTests.test("index<Hashable>(of:)") {
  let s: Set<AnyHashable> = [
    AnyHashable(1010), AnyHashable(2020), AnyHashable(3030.0)
  ]
  expectEqual(AnyHashable(1010), s[s.firstIndex(of: 1010)!])
  expectEqual(AnyHashable(2020), s[s.firstIndex(of: 2020)!])
  expectEqual(AnyHashable(3030.0), s[s.firstIndex(of: 3030.0)!])

  expectNil(s.firstIndex(of: 1010.0))
  expectNil(s.firstIndex(of: 2020.0))
  expectNil(s.firstIndex(of: 3030))
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

runAllTests()

