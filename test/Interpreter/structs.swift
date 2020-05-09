// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


var StructTestSuite = TestSuite("Struct")

struct Interval {
  var lo, hi : Int

  init(_ lo: Int, _ hi: Int) {
    self.lo = lo
    self.hi = hi
  }
}

func +(a: Interval, b: Interval) -> Interval {
  return Interval(a.lo + b.lo, a.hi + b.hi)
}

func -(a: Interval, b: Interval) -> Interval {
  return Interval(a.lo - b.hi, a.hi - b.lo)
}

prefix func -(a: Interval) -> Interval {
  return Interval(-a.hi, -a.lo)
}

func print(_ a: Interval) {
  print("[\(a.lo), \(a.hi)]")
}

StructTestSuite.test("Interval") {
  do {
    let i = -Interval(1, 2)
    expectEqual(-2, i.lo)
    expectEqual(-1, i.hi)
  }
  do {
    let i = Interval(1, 2) + Interval(3, 4)
    expectEqual(4, i.lo)
    expectEqual(6, i.hi)
  }
  do {
    let i = Interval(3, 4) - Interval(1, 2)
    expectEqual(1, i.lo)
    expectEqual(3, i.hi)
  }
}

struct BigStruct {
  var a,b,c,d,e,f,g,h : Int
}

func returnBigStruct() -> BigStruct {
  return BigStruct(a: 1, b: 6, c: 1, d: 8, e: 0, f: 3, g: 4, h: 0)
}

StructTestSuite.test("Big") {
  let bs = returnBigStruct()

  expectEqual(1, bs.a)
  expectEqual(6, bs.b)
  expectEqual(1, bs.c)
  expectEqual(8, bs.d)
  expectEqual(0, bs.e)
  expectEqual(3, bs.f)
  expectEqual(4, bs.g)
  expectEqual(0, bs.h)
}

struct GenStruct<T> {
  var a, b: Int

  init(_ a: Int, _ b: Int) {
    self.a = a
    self.b = b
  }
}

StructTestSuite.test("Generic") {
  let gs = GenStruct<String>(19, 84)
  expectEqual(19, gs.a)
  expectEqual(84, gs.b)
}

struct InitStruct {
  var x: Int
  var y: Int
  var l = LifetimeTracked(0)

  init() {
    x = 10
    y = 20
  }

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  init(b: Bool) {
    if b {
      self = InitStruct(x: 5, y: 7)
    } else {
      self.init(x: 6, y: 8)
    }
  }

  init(initThenAssign: ()) {
    self.init(x: 5, y: 7)
    self = InitStruct(x: 6, y: 8)
  }

  init(assignThenInit: ()) {
    self = InitStruct(x: 5, y: 7)
    self.init(x: 6, y: 8)
  }

  init(initThenInit: ()) {
    self.init(x: 5, y: 7)
    self.init(x: 6, y: 8)
  }
}

StructTestSuite.test("InitStruct") {
  do {
    let s = InitStruct()
    expectEqual(10, s.x)
    expectEqual(20, s.y)
  }
  do {
    let s = InitStruct(x: 69, y: 420)
    expectEqual(69, s.x)
    expectEqual(420, s.y)
  }
  do {
    let s = InitStruct(initThenAssign: ())
    expectEqual(6, s.x)
    expectEqual(8, s.y)
  }
  do {
    let s = InitStruct(assignThenInit: ())
    expectEqual(6, s.x)
    expectEqual(8, s.y)
  }
  do {
    let s = InitStruct(initThenInit: ())
    expectEqual(6, s.x)
    expectEqual(8, s.y)
  }
}

struct InitStructAddrOnly {
  var x: Int
  var y: Int
  var a: Any = "hi"
  var l = LifetimeTracked(0)

  init() {
    x = 10
    y = 20
  }

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  init(b: Bool) {
    if b {
      self = InitStructAddrOnly(x: 5, y: 7)
    } else {
      self.init(x: 6, y: 8)
    }
  }

  init(initThenAssign: ()) {
    self.init(x: 5, y: 7)
    self = InitStructAddrOnly(x: 6, y: 8)
  }

  init(assignThenInit: ()) {
    self = InitStructAddrOnly(x: 5, y: 7)
    self.init(x: 6, y: 8)
  }

  init(initThenInit: ()) {
    self.init(x: 5, y: 7)
    self.init(x: 6, y: 8)
  }
}

StructTestSuite.test("InitStructAddrOnly") {
  do {
    let s = InitStructAddrOnly()
    expectEqual(10, s.x)
    expectEqual(20, s.y)
  }
  do {
    let s = InitStructAddrOnly(x: 69, y: 420)
    expectEqual(69, s.x)
    expectEqual(420, s.y)
  }
  do {
    let s = InitStructAddrOnly(initThenAssign: ())
    expectEqual(6, s.x)
    expectEqual(8, s.y)
  }
  do {
    let s = InitStructAddrOnly(assignThenInit: ())
    expectEqual(6, s.x)
    expectEqual(8, s.y)
  }
  do {
    let s = InitStructAddrOnly(initThenInit: ())
    expectEqual(6, s.x)
    expectEqual(8, s.y)
  }
}

runAllTests()
