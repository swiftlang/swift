// RUN: %target-run-simple-swift | FileCheck %s

var x : Optional<Int> = nil
if x { 
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

if x {
  println("a non-empty optional is logically true") 
} else { 
  assert(false, "x is empty!")
}
// CHECK: a non-empty optional is logically true

if !x { 
  println("logical negation fails 0")
}
else { 
  println("logical negation works 0") 
}
// CHECK: logical negation works 0

if true {
  var y1 : Optional<Int> = .None
  if !y1 {
    println("y1 is .None")
  }
  // CHECK: y1 is .None

  var y2 : Optional<Int> = .None
  if !y2 {
    println("y2 is .None")
  }
  // CHECK: y2 is .None
}

func optional_param(x: Optional<Int>) {
  if !x {
    println("optional param OK")
  }
}
optional_param(.None)
// CHECK: optional param OK

func optional_return() -> Optional<Int> {
  return .None
}
if !optional_return() {
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

println("forced extraction use: \(x!.succ()).")
// CHECK-NEXT: forced extraction use: 2.

func testRelation(p: (Int?, Int?) -> Bool) {
  typealias optPair = (Int?, Int?)
  
  let relationships: optPair[] = [
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
