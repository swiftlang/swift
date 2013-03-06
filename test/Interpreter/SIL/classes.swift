// RUN: %swift -sil-i %s | FileCheck %s

class Interval {
  var lo, hi : Int

  constructor(lo:Int, hi:Int) {
    this.lo = lo
    this.hi = hi
  }
}

func +(a:Interval, b:Interval) -> Interval {
  return new Interval(a.lo + b.lo, a.hi + b.hi)
}

func -(a:Interval, b:Interval) -> Interval {
  return new Interval(a.lo - b.hi, a.hi - b.lo)
}

func -(a:Interval) -> Interval {
  return new Interval(-a.hi, -a.lo)
}

func println(a:Interval) {
  println("[\(a.lo), \(a.hi)]")
}

// CHECK: [-2, -1]
println(-new Interval(1,2))
// CHECK: [4, 6]
println(new Interval(1,2) + new Interval(3,4))
// CHECK: [1, 3]
println(new Interval(3,4) - new Interval(1,2))
