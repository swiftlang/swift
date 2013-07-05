// RUN: %swift -i %s | FileCheck %s

class Interval {
  var lo, hi : Int

  constructor(lo:Int, hi:Int) {
    this.lo = lo
    this.hi = hi
  }

  func show() {
    println("[\(lo), \(hi)]")
  }
  
  static func like(lo:Int, hi:Int) -> Interval {
    return Interval(lo, hi)
  }
}

class OpenInterval : Interval {
  constructor(lo:Int, hi:Int) {
    super.constructor(lo, hi)
  }

  func show() {
    println("(\(lo), \(hi))")
  }

  static func like(lo:Int, hi:Int) -> Interval {
    return OpenInterval(lo, hi)
  }
}

func +(a:Interval, b:Interval) -> Interval {
  return Interval(a.lo + b.lo, a.hi + b.hi)
}

func -(a:Interval, b:Interval) -> Interval {
  return Interval(a.lo - b.hi, a.hi - b.lo)
}

func [prefix] -(a:Interval) -> Interval {
  return typeof(a).like(-a.hi, -a.lo)
}

// CHECK: [-2, -1]
(-Interval(1,2)).show()
// CHECK: [4, 6]
(Interval(1,2) + Interval(3,4)).show()
// CHECK: [1, 3]
(Interval(3,4) - Interval(1,2)).show()
// CHECK: (-1, 1)
(OpenInterval(-1,1)).show()
// CHECK: (-3, 2)
(-OpenInterval(-2,3)).show()

// CHECK: false
println(Interval(1,2) is OpenInterval)
// CHECK: true
println((OpenInterval(1,2) as Interval) is OpenInterval)
