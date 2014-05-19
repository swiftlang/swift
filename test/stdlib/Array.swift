// RUN: %target-run-simple-swift | FileCheck %s

println("testing...")
// CHECK: testing

println(sizeofValue([ 0 ]) - sizeofValue(0))
// CHECK-NEXT: 0

func testCountAndIsEmpty() {
  var emptyVec = Array<String>()
  // CHECK-NEXT: {{^}}emptyVec: 0 true{{$}}
  println("emptyVec: \(emptyVec.count) \(emptyVec.isEmpty)")

  var nonEmptyVec = Array<String>()
  nonEmptyVec.append("1")
  nonEmptyVec.append("2")
  // CHECK-NEXT: {{^}}nonEmptyVec: 2 false{{$}}
  println("nonEmptyVec: \(nonEmptyVec.count) \(nonEmptyVec.isEmpty)")
}

testCountAndIsEmpty()

var primes0 = [2, 3, 5, 7, 11]

func testTakeActualArray() {
  // Copy primes0 into a Array
  var primes1 = Array<Int>()
  for x in primes0 { primes1.append(x) }

  // Take the elements of primes1 in an Array
  var primes2 : Int[] = primes1

  // Check that the copy worked
  // CHECK-NEXT: <2> <3> <5> <7> <11> .
  for x in primes2 { print("<\(x)> ") }
  println(".")
}

testTakeActualArray()
println("done.") // CHECK-NEXT: done.

// Check that we loop over the size of the vector, not its capacity.
// Found by Anna.
var fullButStarvingArray = Array<String>()
for x in fullButStarvingArray { print("\(x) ") }

func testArrayFromStream() {
  var x = Array(3..7)
  // CHECK-NEXT: <3> <4> <5> <6> .
  for y in x { print("<\(y)> ") }
  println(".")
}
testArrayFromStream()

func testMap() {
  var floatPrimes = primes0.map { Float($0) }
  // CHECK-NEXT: <2.0> <3.0> <5.0> <7.0> <11.0> .
  for y in floatPrimes { print("<\(y)> ") }
  println(".")
}
testMap()

func testUnshare() {
  var x = [35]
  var y = x
  y.unshare()
  x[0] = 8
  print("<\(y[0])>\n")
  // CHECK-NEXT: <35>
}
testUnshare()

func testCopy() {
  var x = [320]
  var y = x.copy()
  x[0] = 7
  print("<\(y[0])>\n")
  // CHECK-NEXT: <320>
}
testCopy()

func testFilter() {
  let a = Array(3..16)
  println(a.filter { $0 % 4 == 0 })
  // CHECK-NEXT: [4, 8, 12]
}
testFilter()

func testReverse() {
  let a = Array(7..12)
  println(a.reverse())
  // CHECK-NEXT: [11, 10, 9, 8, 7]
}
testReverse()

println("done!") // CHECK-NEXT: {{^done!$}}

