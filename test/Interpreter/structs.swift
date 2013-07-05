// RUN: %swift -i %s | FileCheck %s

struct Interval {
  var lo, hi : Int
}

func +(a:Interval, b:Interval) -> Interval {
  return Interval(a.lo + b.lo, a.hi + b.hi)
}

func -(a:Interval, b:Interval) -> Interval {
  return Interval(a.lo - b.hi, a.hi - b.lo)
}

func [prefix] -(a:Interval) -> Interval {
  return Interval(-a.hi, -a.lo)
}

func println(a:Interval) {
  println("[\(a.lo), \(a.hi)]")
}

// CHECK: [-2, -1]
println(-Interval(1,2))
// CHECK: [4, 6]
println(Interval(1,2) + Interval(3,4))
// CHECK: [1, 3]
println(Interval(3,4) - Interval(1,2))

// CHECK: And now you know
println("And now you know the rest of the story")

struct BigStruct {
  var a,b,c,d,e,f,g,h : Int
}

func returnBigStruct() -> BigStruct {
  return BigStruct(1,6,1,8,0,3,4,0)
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
println(bs.a)
println(bs.b)
println(bs.c)
println(bs.d)
println(bs.e)
println(bs.f)
println(bs.g)
println(bs.h)

struct GenStruct<T> {
  var a, b : Int
}

// CHECK: 19
// CHECK: 84
var gs = GenStruct<String>(19, 84)
println(gs.a)
println(gs.b)
