// RUN: %target-run-simple-swift | FileCheck %s

typealias Interval = (lo: Int, hi: Int)

infix operator <+> {}
infix operator <-> {}
infix operator <+>= {}

func <+>(a: Interval, b: Interval) -> Interval {
  return (a.lo + b.lo, a.hi + b.hi)
}

func <->(a: Interval, b: Interval) -> Interval {
  return (a.lo - b.hi, a.hi - b.lo)
}

func <+>=(inout a: Interval, b: Interval) {
  a.lo += b.lo
  a.hi += b.hi
}

func println(x: Interval) {
  println("(lo=\(x.lo), hi=\(x.hi))")
}

// CHECK: (lo=4, hi=6)
println((1,2) <+> (3,4))
// CHECK: (lo=4, hi=6)
println((hi:2,lo:1) <+> (lo:3,hi:4))
// CHECK: (lo=1, hi=3)
println((3,4) <-> (1,2))

func mutate() {
  var x:Interval = (1, 2)
  x <+>= (3, 4)
  // CHECK: (lo=4, hi=6)
  println(x)
}
mutate()

func printInts(ints: Int...) {
  print("\(ints.count) ints: ")
  for int in ints {
    print("\(int) ")
  }
  print("\n")
}

// CHECK: 0 ints
printInts()
// CHECK: 1 ints: 1
printInts(1)
// CHECK: 3 ints: 1 2 3
printInts(1,2,3)
