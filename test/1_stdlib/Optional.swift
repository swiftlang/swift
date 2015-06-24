// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

var x : Optional<Int> = nil
if x != nil { 
  print("x is non-empty!")
}
else { 
  print("an empty optional is logically false")
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
  print("a non-empty optional is logically true") 
} else { 
  assert(false, "x is empty!")
}
// CHECK: a non-empty optional is logically true

if x == nil { 
  print("logical negation fails 0")
}
else { 
  print("logical negation works 0") 
}
// CHECK: logical negation works 0

if true {
  var y1 : Optional<Int> = .None
  if y1 == nil {
    print("y1 is .None")
  }
  // CHECK: y1 is .None

  var y2 : Optional<Int> = .None
  if y2 == nil {
    print("y2 is .None")
  }
  // CHECK: y2 is .None
}

func optional_param(x: Optional<Int>) {
  if x == nil {
    print("optional param OK")
  }
}
optional_param(.None)
// CHECK: optional param OK

func optional_return() -> Optional<Int> {
  return .None
}
if optional_return() == nil {
  print("optional return OK")
}
// CHECK: optional return OK

var empty: Bool = true
switch x {
case .Some(var y):
  print("destructuring bind: \(y).")
case .None:
  ()
}
// CHECK: destructuring bind: 1.


print("forced extraction: \(x!).")
// CHECK: forced extraction: 1.

print("forced extraction use: \(x!.successor()).")
// CHECK-NEXT: forced extraction use: 2.

func testRelation(p: (Int?, Int?) -> Bool) {
  typealias optPair = (Int?, Int?)
  
  let relationships: [optPair] = [
    (1, 1), (1, 2), (2, 1), (1, .None), (.None, 1), (.None, .None)
  ]

  var prefix = ""
  for (l,r) in relationships {
    print("\(prefix)\(p(l, r))", appendNewline: false)
    prefix=", "
  }
  print(".")
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
func == (_: E, _: E) -> Bool { return true }

func nilComparison() {
  let x0: X? = nil
  let x1: X? = X()

  /*
  // FIXME: <rdar://problem/17489239> Optional<T>() == nil where T: !Equatable
  print(x1 == nil) // DISABLED-CHECK-NEXT: false
  print(x1 != nil) // DISABLED-CHECK-NEXT: true
  print(x0 == nil) // DISABLED-CHECK-NEXT: true
  print(x0 != nil) // DISABLED-CHECK-NEXT: false

  print(nil == x1) // DISABLED-CHECK-NEXT: false
  print(nil != x1) // DISABLED-CHECK-NEXT: true
  print(nil == x0) // DISABLED-CHECK-NEXT: true
  print(nil != x0) // DISABLED-CHECK-NEXT: false
  */
  
  let v0: Int? = nil
  let v1: Int? = 1
  
  print(v1 == nil) // CHECK-NEXT: false
  print(v1 != nil) // CHECK-NEXT: true
  print(v0 == nil) // CHECK-NEXT: true
  print(v0 != nil) // CHECK-NEXT: false

  print(nil == v1) // CHECK-NEXT: false
  print(nil != v1) // CHECK-NEXT: true
  print(nil == v0) // CHECK-NEXT: true
  print(nil != v0) // CHECK-NEXT: false

  let c0: C? = nil
  let c1: C? = C()
  
  /*
  // FIXME: <rdar://problem/17489239> Optional<T>() == nil where T: !Equatable
  print(c1 == nil) // DISABLED-CHECK-NEXT: false
  print(c1 != nil) // DISABLED-CHECK-NEXT: true
  print(c0 == nil) // DISABLED-CHECK-NEXT: true
  print(c0 != nil) // DISABLED-CHECK-NEXT: false

  print(nil == c1) // DISABLED-CHECK-NEXT: false
  print(nil != c1) // DISABLED-CHECK-NEXT: true
  print(nil == c0) // DISABLED-CHECK-NEXT: true
  print(nil != c0) // DISABLED-CHECK-NEXT: false
  */
  
  let e0: E? = nil
  let e1: E? = E()
  
  print(e1 == nil) // CHECK-NEXT: false
  print(e1 != nil) // CHECK-NEXT: true
  print(e0 == nil) // CHECK-NEXT: true
  print(e0 != nil) // CHECK-NEXT: false

  print(nil == e1) // CHECK-NEXT: false
  print(nil != e1) // CHECK-NEXT: true
  print(nil == e0) // CHECK-NEXT: true
  print(nil != e0) // CHECK-NEXT: false
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

debugPrint(a ?? nextCounter())      // CHECK-NEXT: 123
debugPrint(b ?? nextCounter())      // CHECK-NEXT: 0
debugPrint(c ?? nextCounter())      // CHECK-NEXT: 1
debugPrint(d ?? nextCounter())      // CHECK-NEXT: 456
debugPrint(e ?? d ?? nextCounter()) // CHECK-NEXT: 456
debugPrint(f ?? nextCounter())      // CHECK-NEXT: 2

func nextCounter2() -> Int? {
  return nextCounter()
}

debugPrint(c ?? d)                   // CHECK-NEXT: Optional(456)
debugPrint(c ?? e)                   // CHECK-NEXT: nil
debugPrint(a ?? nextCounter2())      // CHECK-NEXT: Optional(123)
debugPrint(b ?? nextCounter2())      // CHECK-NEXT: Optional(3)
debugPrint(c ?? nextCounter2())      // CHECK-NEXT: Optional(4)
debugPrint(d ?? nextCounter2())      // CHECK-NEXT: Optional(456)
debugPrint(e ?? d ?? nextCounter2()) // CHECK-NEXT: Optional(456)
debugPrint(f ?? nextCounter2())      // CHECK-NEXT: Optional(5)

import StdlibUnittest
import Swift

var OptionalTests = TestSuite("Optional")

OptionalTests.test("flatMap") {
  let half: Int32 -> Int16? =
    { if $0 % 2 == 0 { return Int16($0 / 2) } else { return .None } }

  expectOptionalEqual(2 as Int16, half(4))
  expectEmpty(half(3))

  expectEmpty((.None as Int32?).flatMap(half))
  expectOptionalEqual(2 as Int16, (4 as Int32?).flatMap(half))
  expectEmpty((3 as Int32?).flatMap(half))
}

runAllTests()
