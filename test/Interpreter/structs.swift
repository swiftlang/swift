// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

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

func print(a: Interval) {
  print("[\(a.lo), \(a.hi)]")
}

// CHECK: [-2, -1]
print(-Interval(1,2))
// CHECK: [4, 6]
print(Interval(1,2) + Interval(3,4))
// CHECK: [1, 3]
print(Interval(3,4) - Interval(1,2))

// CHECK: And now you know
print("And now you know the rest of the story")

struct BigStruct {
  var a,b,c,d,e,f,g,h : Int
}

func returnBigStruct() -> BigStruct {
  return BigStruct(a: 1, b: 6, c: 1, d: 8, e: 0, f: 3, g: 4, h: 0)
}

// CHECK: 1
// CHECK: 6
// CHECK: 1
// CHECK: 8
// CHECK: 0
// CHECK: 3
// CHECK: 4
// CHECK: 0
var bs = returnBigStruct()
print(bs.a)
print(bs.b)
print(bs.c)
print(bs.d)
print(bs.e)
print(bs.f)
print(bs.g)
print(bs.h)

struct GenStruct<T> {
  var a, b : Int

  init(_ a: Int, _ b: Int) {
    self.a = a
    self.b = b
  }

}

// CHECK: 19
// CHECK: 84
var gs = GenStruct<String>(19, 84)
print(gs.a)
print(gs.b)
