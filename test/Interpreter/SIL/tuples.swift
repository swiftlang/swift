// RUN: %swift -no-constraint-checker -sil-i %s | FileCheck %s

typealias Interval = (lo:Int, hi:Int)

func [infix_left=190] <+>(a:Interval, b:Interval) -> Interval {
  return (a.lo + b.lo, a.hi + b.hi)
}

func [infix_left=190] <->(a:Interval, b:Interval) -> Interval {
  return (a.lo - b.hi, a.hi - b.lo)
}

func [assignment,infix] <+>=(a:[byref] Interval, b:Interval) {
  a.lo += b.lo
  a.hi += b.hi
}

func println(x:Interval) {
  println("(lo=\(x.lo), hi=\(x.hi))")
}

// CHECK: (lo=4, hi=6)
println((1,2) <+> (3,4))
// CHECK: (lo=4, hi=6)
println((hi=2,lo=1) <+> (lo=3,hi=4))
// CHECK: (lo=1, hi=3)
println((3,4) <-> (1,2))

func mutate() {
  var x:Interval = (1, 2)
  x <+>= (3, 4)
  // CHECK: (lo=4, hi=6)
  println(x)
}
mutate()

/* FIXME: specialize not supported yet by SIL-IRGen, blocks support for
   making slices
func printInts(ints:Int...) {
  print("\(ints.length) ints: ")
  for int in ints {
    print("\(int) ")
  }
  print("\n")
}

// C/HECK: 0 ints
printInts()
// C/HECK: 1 ints: 1
printInts(1)
// C/HECK: 3 ints: 1 2 3
printInts(1,2,3)
*/
