// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var DictionaryTests = TestSuite("Dictionary")

DictionaryTests.test("index<Hashable>(forKey:)") {
  let d: [AnyHashable : Int] = [
    AnyHashable(10) : 1010,
    AnyHashable(20) : 2020,
    AnyHashable(30.0) : 3030,
  ]

  expectEqual(1010, d[d.index(forKey: 10)!].value)
  expectEqual(2020, d[d.index(forKey: 20)!].value)
  expectEqual(3030, d[d.index(forKey: 30.0)!].value)

  expectEmpty(d.index(forKey: 10.0))
  expectEmpty(d.index(forKey: 20.0))
  expectEmpty(d.index(forKey: 30))
}

DictionaryTests.test("subscript<Hashable>(_:)") {
  var d: [AnyHashable : Int] = [
    AnyHashable(10) : 1010,
    AnyHashable(20) : 2020,
    AnyHashable(30.0) : 3030,
  ]

  expectEqual(1010, d[10])
  expectEqual(2020, d[20])
  expectEqual(3030, d[30.0])

  d[10] = 101010
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 2020,
      AnyHashable(30.0) : 3030,
    ]
    expectEqual(expected, d)
  }

  d[20] = 202020
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 202020,
      AnyHashable(30.0) : 3030,
    ]
    expectEqual(expected, d)
  }

  d[30.0] = 303030
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 202020,
      AnyHashable(30.0) : 303030,
    ]
    expectEqual(expected, d)
  }
}

DictionaryTests.test("updateValue<Hashable>(_:forKey:)") {
  var d: [AnyHashable : Int] = [
    AnyHashable(10) : 1010,
    AnyHashable(20) : 2020,
    AnyHashable(30.0) : 3030,
  ]

  expectEqual(1010, d.updateValue(101010, forKey: 10)!)
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 2020,
      AnyHashable(30.0) : 3030,
    ]
    expectEqual(expected, d)
  }

  expectEqual(2020, d.updateValue(202020, forKey: 20)!)
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 202020,
      AnyHashable(30.0) : 3030,
    ]
    expectEqual(expected, d)
  }

  expectEqual(3030, d.updateValue(303030, forKey: 30.0)!)
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 202020,
      AnyHashable(30.0) : 303030,
    ]
    expectEqual(expected, d)
  }

  expectEmpty(d.updateValue(4040, forKey: 10.0))
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 202020,
      AnyHashable(30.0) : 303030,
      AnyHashable(10.0) : 4040,
    ]
    expectEqual(expected, d)
  }

  expectEmpty(d.updateValue(5050, forKey: 20.0))
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 202020,
      AnyHashable(30.0) : 303030,
      AnyHashable(10.0) : 4040,
      AnyHashable(20.0) : 5050,
    ]
    expectEqual(expected, d)
  }

  expectEmpty(d.updateValue(6060, forKey: 30))
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 101010,
      AnyHashable(20) : 202020,
      AnyHashable(30.0) : 303030,
      AnyHashable(10.0) : 4040,
      AnyHashable(20.0) : 5050,
      AnyHashable(30) : 6060,
    ]
    expectEqual(expected, d)
  }
}

DictionaryTests.test("removeValue<Hashable>(forKey:)") {
  var d: [AnyHashable : Int] = [
    AnyHashable(10) : 1010,
    AnyHashable(20) : 2020,
    AnyHashable(30.0) : 3030,
  ]

  expectEmpty(d.removeValue(forKey: 10.0))
  expectEmpty(d.removeValue(forKey: 20.0))
  expectEmpty(d.removeValue(forKey: 30))

  expectEqual(1010, d.removeValue(forKey: 10)!)
  expectEqual(2020, d.removeValue(forKey: 20)!)
  expectEqual(3030, d.removeValue(forKey: 30.0)!)
}

runAllTests()

