// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

func testCountAndIsEmpty() {
  var emptyVec = Array<String>()
  // CHECK: {{^}}emptyVec: 0 true{{$}}
  println("emptyVec: \(emptyVec.count) \(emptyVec.isEmpty)")

  var nonEmptyVec = Array<String>()
  nonEmptyVec.append("1")
  nonEmptyVec.append("2")
  // CHECK: {{^}}nonEmptyVec: 2 false{{$}}
  println("nonEmptyVec: \(nonEmptyVec.count) \(nonEmptyVec.isEmpty)")
}

testCountAndIsEmpty()

var primes0 = [2, 3, 5, 7, 11]

func testTakeActualArray() {
  // Copy primes0 into a Array
  var primes1 = Array<Int>()
  for x in primes0 { primes1.append(x) }

  // Take the elements of primes1 in an Array
  var primes2 : Int[] = primes1.takeArray()

  // Check that takeArray leaves primes1 empty
  // CHECK: <0>
  println("<\(primes1.count)>")

  // Check that we got the right elements from takeArray
  // CHECK: <2> <3> <5> <7> <11> .
  for x in primes2 { print("<\(x)> ") }
  println(".")
}

testTakeActualArray()
println("done.") // CHECK: done.

// Check that we loop over the size of the vector, not its capacity.
// Found by Anna.
var fullButStarvingArray = Array<String>()
fullButStarvingArray.reserve(4)
for x in fullButStarvingArray { print("\(x) ") }
println("done!") // CHECK: {{^done!$}}
