// RUN: %target-run-simple-swift | FileCheck %s

// XFAIL: linux

var x : Optional<Int> = nil
if x != nil { 
  println("x is non-empty!")
}
else { 
  println("an empty optional is logically false")
}
// CHECK: an empty optional is logically false

switch x {
case .Some(var y):
  assert(false, "Something's wrong here!")
case .None:
  ()
}

x = .Some(0)

x = .Some(1)

if x != nil {
  println("a non-empty optional is logically true") 
} else { 
  assert(false, "x is empty!")
}
// CHECK: a non-empty optional is logically true

if x == nil { 
  println("logical negation fails 0")
}
else { 
  println("logical negation works 0") 
}
// CHECK: logical negation works 0

if true {
  var y1 : Optional<Int> = .None
  if y1 == nil {
    println("y1 is .None")
  }
  // CHECK: y1 is .None

  var y2 : Optional<Int> = .None
  if y2 == nil {
    println("y2 is .None")
  }
  // CHECK: y2 is .None
}

func optional_param(x: Optional<Int>) {
  if x == nil {
    println("optional param OK")
  }
}
optional_param(.None)
// CHECK: optional param OK

func optional_return() -> Optional<Int> {
  return .None
}
if optional_return() == nil {
  println("optional return OK")
}
// CHECK: optional return OK

var empty: Bool = true
switch x {
case .Some(var y):
  println("destructuring bind: \(y).")
case .None:
  ()
}
// CHECK: destructuring bind: 1.


println("forced extraction: \(x!).")
// CHECK: forced extraction: 1.

println("forced extraction use: \(x!.successor()).")
// CHECK-NEXT: forced extraction use: 2.

func testRelation(p: (Int?, Int?) -> Bool) {
  typealias optPair = (Int?, Int?)
  
  let relationships: [optPair] = [
    (1, 1), (1, 2), (2, 1), (1, .None), (.None, 1), (.None, .None)
  ]

  var prefix = ""
  for (l,r) in relationships {
    print("\(prefix)\(p(l, r))")
    prefix=", "
  }
  println(".")
}

testRelation(==)
// CHECK-NEXT: true, false, false, false, false, true.

testRelation(!=)
// CHECK-NEXT: false, true, true, true, true, false

testRelation(<)
// CHECK-NEXT: false, true, false, false, true, false.

struct X {}
class C {}

class E : Equatable {}
func == (E, E) -> Bool { return true }

func nilComparison() {
  let x0: X? = nil
  let x1: X? = X()

  /*
  // FIXME: <rdar://problem/17489239> Optional<T>() == nil where T: !Equatable
  println(x1 == nil) // DISABLED-CHECK-NEXT: false
  println(x1 != nil) // DISABLED-CHECK-NEXT: true
  println(x0 == nil) // DISABLED-CHECK-NEXT: true
  println(x0 != nil) // DISABLED-CHECK-NEXT: false

  println(nil == x1) // DISABLED-CHECK-NEXT: false
  println(nil != x1) // DISABLED-CHECK-NEXT: true
  println(nil == x0) // DISABLED-CHECK-NEXT: true
  println(nil != x0) // DISABLED-CHECK-NEXT: false
  */
  
  let v0: Int? = nil
  let v1: Int? = 1
  
  println(v1 == nil) // CHECK-NEXT: false
  println(v1 != nil) // CHECK-NEXT: true
  println(v0 == nil) // CHECK-NEXT: true
  println(v0 != nil) // CHECK-NEXT: false

  println(nil == v1) // CHECK-NEXT: false
  println(nil != v1) // CHECK-NEXT: true
  println(nil == v0) // CHECK-NEXT: true
  println(nil != v0) // CHECK-NEXT: false

  let c0: C? = nil
  let c1: C? = C()
  
  /*
  // FIXME: <rdar://problem/17489239> Optional<T>() == nil where T: !Equatable
  println(c1 == nil) // DISABLED-CHECK-NEXT: false
  println(c1 != nil) // DISABLED-CHECK-NEXT: true
  println(c0 == nil) // DISABLED-CHECK-NEXT: true
  println(c0 != nil) // DISABLED-CHECK-NEXT: false

  println(nil == c1) // DISABLED-CHECK-NEXT: false
  println(nil != c1) // DISABLED-CHECK-NEXT: true
  println(nil == c0) // DISABLED-CHECK-NEXT: true
  println(nil != c0) // DISABLED-CHECK-NEXT: false
  */
  
  let e0: E? = nil
  let e1: E? = E()
  
  println(e1 == nil) // CHECK-NEXT: false
  println(e1 != nil) // CHECK-NEXT: true
  println(e0 == nil) // CHECK-NEXT: true
  println(e0 != nil) // CHECK-NEXT: false

  println(nil == e1) // CHECK-NEXT: false
  println(nil != e1) // CHECK-NEXT: true
  println(nil == e0) // CHECK-NEXT: true
  println(nil != e0) // CHECK-NEXT: false
}
nilComparison()

var counter = 0
func nextCounter() -> Int {
  return counter++
}

let a: Int? = 123
let b: Int? = nil
let c: Int? = nil
let d: Int? = 456
let e: Int? = nil
let f: Int? = nil

debugPrintln(a ?? nextCounter())      // CHECK-NEXT: 123
debugPrintln(b ?? nextCounter())      // CHECK-NEXT: 0
debugPrintln(c ?? nextCounter())      // CHECK-NEXT: 1
debugPrintln(d ?? nextCounter())      // CHECK-NEXT: 456
debugPrintln(e ?? d ?? nextCounter()) // CHECK-NEXT: 456
debugPrintln(f ?? nextCounter())      // CHECK-NEXT: 2

func nextCounter2() -> Int? {
  return nextCounter()
}

debugPrintln(c ?? d)                   // CHECK-NEXT: Optional(456)
debugPrintln(c ?? e)                   // CHECK-NEXT: nil
debugPrintln(a ?? nextCounter2())      // CHECK-NEXT: Optional(123)
debugPrintln(b ?? nextCounter2())      // CHECK-NEXT: Optional(3)
debugPrintln(c ?? nextCounter2())      // CHECK-NEXT: Optional(4)
debugPrintln(d ?? nextCounter2())      // CHECK-NEXT: Optional(456)
debugPrintln(e ?? d ?? nextCounter2()) // CHECK-NEXT: Optional(456)
debugPrintln(f ?? nextCounter2())      // CHECK-NEXT: Optional(5)

import StdlibUnittest
import Swift

var OptionalTests = TestSuite("Optional")

let half : Int -> Int? =
  { if $0 % 2 == 0 { return $0 / 2 } else { return .None } }

OptionalTests.test("flatMap") {
  // FIXME: type inference stops expectEqual from working
  expectTrue((.None as Int?).flatMap(half) == .None)
  expectTrue(half(4) == .Some(2))
  expectTrue(half(4).flatMap(half) == .Some(1))
  expectTrue(half(4).flatMap(half).flatMap(half) == .None)
}

runAllTests()
