// RUN: %swift -sil-i %s | FileCheck %s

struct Interval {
  var lo, hi : Int
}

func +(a:Interval, b:Interval) -> Interval {
  return Interval(a.lo + b.lo, a.hi + b.hi)
}

func -(a:Interval, b:Interval) -> Interval {
  return Interval(a.lo - b.hi, a.hi - b.lo)
}

func -(a:Interval) -> Interval {
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
