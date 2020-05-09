// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

extension MinimalEquatableValue : Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}

public struct DefaultedSetAlgebra<Element : Hashable> : SetAlgebra {
  var store = Set<Element>()
  public init() {}

  public init(_ elements: [Element]) {
    store = Set(elements)
  }

  public init(_store: Set<Element>) {
    store = _store
  }

  public func contains(_ member: Element) -> Bool {
    return store.contains(member)
  }

  public func union(_ other: DefaultedSetAlgebra) -> DefaultedSetAlgebra {
    return DefaultedSetAlgebra(_store: store.union(other.store))
  }

  public func intersection(_ other: DefaultedSetAlgebra)
    -> DefaultedSetAlgebra {
    var defaultedSetAlgebra = DefaultedSetAlgebra()
    defaultedSetAlgebra.store = store.intersection(other.store)
    return defaultedSetAlgebra
  }

  public func symmetricDifference(_ other: DefaultedSetAlgebra)
    -> DefaultedSetAlgebra {
    var defaultedSetAlgebra = DefaultedSetAlgebra()
    defaultedSetAlgebra.store = store.symmetricDifference(other.store)
    return defaultedSetAlgebra
  }

  public mutating func insert(_ newMember: Element)
    -> (inserted: Bool, memberAfterInsert: Element) {
    return store.insert(newMember)
  }

  public mutating func remove(_ member: Element) -> Element? {
    return store.remove(member)
  }

  public mutating func update(with newMember: Element) -> Element? {
    return store.update(with: newMember)
  }

  public mutating func formUnion(_ other: DefaultedSetAlgebra) {
    store.formUnion(other.store)
  }

  public mutating func formSymmetricDifference(_ other: DefaultedSetAlgebra) {
    store.formSymmetricDifference(other.store)
  }

  public mutating func formIntersection(_ other: DefaultedSetAlgebra) {
    store.formIntersection(other.store)
  }
}

public func ==<Element>(lhs: DefaultedSetAlgebra<Element>,
  rhs: DefaultedSetAlgebra<Element>) -> Bool {
  return lhs.store == rhs.store
}

let SetAlgebraTests = TestSuite("SetAlgebra")

SetAlgebraTests.test("contains").forEach(in: findTests) { test in
  typealias P = DefaultedSetAlgebra<MinimalEquatableValue>
  expectEqual(test.expected != nil,
    P(test.sequence).contains(test.element))
}

SetAlgebraTests.test("union").forEach(in: unionTests) { test in
  expectEqual(DefaultedSetAlgebra(test.expected),
    DefaultedSetAlgebra(test.lhs).union(DefaultedSetAlgebra(test.rhs)))
}

SetAlgebraTests.test("intersection").forEach(in: intersectionTests) { test in
  expectEqual(DefaultedSetAlgebra(test.expected),
    DefaultedSetAlgebra(test.lhs).intersection(DefaultedSetAlgebra(test.rhs)))
}

SetAlgebraTests.test("symmetricDifference")
  .forEach(in: symmetricDifferenceTests) { test in
  let sequence =  DefaultedSetAlgebra(test.rhs)
  expectEqual(DefaultedSetAlgebra(test.expected),
    DefaultedSetAlgebra(test.lhs).symmetricDifference(sequence))
}

SetAlgebraTests.test("insert").forEach(in: findTests) { test in
  var sequence = DefaultedSetAlgebra(test.sequence)
  expectEqual((test.expected == nil, test.element),
    sequence.insert(test.element))
}

SetAlgebraTests.test("subtracting").forEach(in: subtractingTests) { test in
  expectEqual(DefaultedSetAlgebra(test.expected),
    DefaultedSetAlgebra(test.lhs).subtracting(DefaultedSetAlgebra(test.rhs)))
}

SetAlgebraTests.test("isSubset").forEach(in: findTests) { test in
  let sequence = DefaultedSetAlgebra(test.sequence)
  expectEqual(test.expected != nil,
    DefaultedSetAlgebra([test.element]).isSubset(of: sequence))
}

SetAlgebraTests.test("isDisjoint").forEach(in: findTests) { test in
  let sequence = DefaultedSetAlgebra(test.sequence)
  expectEqual(test.expected == nil,
    sequence.isDisjoint(with: DefaultedSetAlgebra([test.element])))
}

SetAlgebraTests.test("isSuperset").forEach(in: findTests) { test in
  let sequence = DefaultedSetAlgebra(test.sequence)
  expectEqual(test.expected != nil,
    sequence.isSuperset(of: DefaultedSetAlgebra([test.element])))
}

SetAlgebraTests.test("isStrictSuperset")
  .forEach(in: strictSupersetTests) { test in
  let sequence = DefaultedSetAlgebra(test.rhs)
  expectEqual(test.expected,
    DefaultedSetAlgebra(test.lhs).isStrictSuperset(of: sequence))
}

SetAlgebraTests.test("isStrictSubset").forEach(in: strictSupersetTests) {
  test in
  let sequence = DefaultedSetAlgebra(test.lhs)
  expectEqual(test.expected,
    DefaultedSetAlgebra(test.rhs).isStrictSubset(of: sequence))
}

SetAlgebraTests.test("isEmpty") {
  let sequence = [MinimalEquatableValue(1, identity: 1),
    MinimalEquatableValue(2, identity: 2)]

  expectTrue(DefaultedSetAlgebra<MinimalEquatableValue>().isEmpty)
  expectFalse(DefaultedSetAlgebra(sequence).isEmpty)
}

SetAlgebraTests.test("subtract").forEach(in: subtractTests) { test in
  var sequence = DefaultedSetAlgebra(test.lhs)
  sequence.subtract(DefaultedSetAlgebra(test.rhs))
  expectEqual(sequence, DefaultedSetAlgebra(test.expected))
}

runAllTests()

