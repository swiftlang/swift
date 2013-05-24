// RUN: %swift -i %s | FileCheck %s
var primes0 = [2, 3, 5, 7, 11]

func testTakeArray() {
  // Copy primes0 into a Vector
  var primes1 = Vector<Int>()
  for x in primes0 { primes1.append(x) }

  // Take the elements of primes1 in an Array
  var primes2 : Int[] = primes1.takeArray()

  // Check that takeArray leaves primes1 empty
  // CHECK: <0>
  println("<\(primes1.length)>")

  // Check that we got the right elements from takeArray
  // CHECK: <2> <3> <5> <7> <11> .
  for x in primes2 { print("<\(x)> ") }
  println(".")
}

testTakeArray()
println("done.")
