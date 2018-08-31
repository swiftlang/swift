// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var DictionaryTests = TestSuite("Dictionary")

DictionaryTests.test("index<Hashable>(forKey:)") {
  let a = AnyHashable(10 as UInt16)
  let b = AnyHashable(20)
  let c = AnyHashable(30.0)
  let d: [AnyHashable: Int] = [
    a: 1010,
    b: 2020,
    c: 3030,
  ]

  for (key, k, value) in [(a, 10, 1010), (b, 20, 2020), (c, 30, 3030)] {
    let index = d.index(forKey: key)!
    expectEqual(value, d[index].value)
    // We must be able to look up the same number in any representation.
    expectEqual(index, d.index(forKey: UInt8(k)))
    expectEqual(index, d.index(forKey: UInt16(k)))
    expectEqual(index, d.index(forKey: UInt32(k)))
    expectEqual(index, d.index(forKey: UInt64(k)))
    expectEqual(index, d.index(forKey: UInt(k)))
    expectEqual(index, d.index(forKey: Int8(k)))
    expectEqual(index, d.index(forKey: Int16(k)))
    expectEqual(index, d.index(forKey: Int32(k)))
    expectEqual(index, d.index(forKey: Int64(k)))
    expectEqual(index, d.index(forKey: Int(k)))
    expectEqual(index, d.index(forKey: Float(k)))
    expectEqual(index, d.index(forKey: Double(k)))

    expectNil(d.index(forKey: String(k)))
  }
}

DictionaryTests.test("subscript<Hashable>(_:)") {
  let a = AnyHashable(10 as UInt16)
  let b = AnyHashable(20)
  let c = AnyHashable(30.0)
  let d: [AnyHashable: Int] = [
    a: 1010,
    b: 2020,
    c: 3030,
  ]

  for (key, k, value) in [(a, 10, 1010), (b, 20, 2020), (c, 30, 3030)] {
    let index = d.index(forKey: key)!
    expectEqual(value, d[key])
    // We must be able to look up the same number in any representation.
    expectEqual(value, d[UInt8(k)])
    expectEqual(value, d[UInt16(k)])
    expectEqual(value, d[UInt32(k)])
    expectEqual(value, d[UInt64(k)])
    expectEqual(value, d[UInt(k)])
    expectEqual(value, d[Int8(k)])
    expectEqual(value, d[Int16(k)])
    expectEqual(value, d[Int32(k)])
    expectEqual(value, d[Int64(k)])
    expectEqual(value, d[Int(k)])
    expectEqual(value, d[Float(k)])
    expectEqual(value, d[Double(k)])

    expectNil(d[String(k)])
  }
}


DictionaryTests.test("subscript<Hashable>(_:)/2") {
  var d: [AnyHashable: Int] = [
    AnyHashable(10): 1010,
    AnyHashable(20): 2020,
    AnyHashable(30.0): 3030,
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

  expectEqual(101010, d.updateValue(4040, forKey: 10.0))
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 4040,
      AnyHashable(20) : 202020,
      AnyHashable(30.0) : 303030,
    ]
    expectEqual(expected, d)
  }

  expectEqual(202020, d.updateValue(5050, forKey: 20.0))
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 4040,
      AnyHashable(20) : 5050,
      AnyHashable(30.0) : 303030,
    ]
    expectEqual(expected, d)
  }

  expectEqual(303030, d.updateValue(6060, forKey: 30))
  do {
    let expected: [AnyHashable : Int] = [
      AnyHashable(10) : 4040,
      AnyHashable(20) : 5050,
      AnyHashable(30.0) : 6060,
    ]
    expectEqual(expected, d)
  }
}

DictionaryTests.test("removeValue<Hashable>(forKey:)") {
  let d: [AnyHashable : Int] = [
    AnyHashable(10 as UInt8) : 1010,
    AnyHashable(20) : 2020,
    AnyHashable(30.0) : 3030,
  ]

  for (key, value) in [(10, 1010), (20, 2020), (30, 3030)] {
    var dd = d
    expectEqual(value, dd.removeValue(forKey: UInt8(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: UInt16(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: UInt32(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: UInt64(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: UInt(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: Int8(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: Int16(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: Int32(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: Int64(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: Int(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: Float(key)))
    dd = d; expectEqual(value, dd.removeValue(forKey: Double(key)))

    dd = d; expectNil(dd.removeValue(forKey: String(key)))
  }
}

runAllTests()

