// RUN: %target-run-simple-swift | FileCheck %s

// XFAIL: linux

import Foundation

print("testing...")
// CHECK: testing

var primes0 = [2, 3, 5, 7, 11]

func testTakeActualArray() {
  // Copy primes0 into a Array
  var primes1 = Array<Int>()
  for x in primes0 { primes1.append(x) }

  // Take the elements of primes1 in an Array
  var primes2 : [Int] = primes1

  // Check that the copy worked
  // CHECK-NEXT: <2> <3> <5> <7> <11> .
  for x in primes2 { print("<\(x)> ", appendNewline: false) }
  print(".")
}

testTakeActualArray()
print("done.") // CHECK-NEXT: done.

// Check that we loop over the size of the vector, not its capacity.
// Found by Anna.
var fullButStarvingArray = Array<String>()
for x in fullButStarvingArray { print("\(x) ", appendNewline: false) }

func testArrayFromStream() {
  var x = Array(3..<7)
  // CHECK-NEXT: <3> <4> <5> <6> .
  for y in x { print("<\(y)> ", appendNewline: false) }
  print(".")
}
testArrayFromStream()

func testMap() {
  var floatPrimes = primes0.map { Float($0) }
  // CHECK-NEXT: <2.0> <3.0> <5.0> <7.0> <11.0> .
  for y in floatPrimes { print("<\(y)> ", appendNewline: false) }
  print(".")
}
testMap()

func testFilter() {
  let a = Array(3..<16)
  print(a.filter { $0 % 4 == 0 })
  // CHECK-NEXT: [4, 8, 12]
}
testFilter()

func testArrayOfVoid() {
  // This used to crash because it generated an array of void with stride == 0
  // CHECK-NEXT: 123
  [1,2,3].map {  
    print($0, appendNewline: false)
  }
  print("")
}
testArrayOfVoid()

func testMutableArray() {
  var m = NSMutableArray(array: ["fu", "bar", "buzz"])
  let a = m as NSArray as! [NSString]
  print(a) // CHECK-NEXT: [fu, bar, buzz]
  m.addObject("goop")
  print(a) // CHECK-NEXT: [fu, bar, buzz]
}
testMutableArray()

print("done!") // CHECK-NEXT: {{^done!$}}

