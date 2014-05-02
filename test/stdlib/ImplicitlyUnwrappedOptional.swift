// RUN: %target-run-simple-swift | FileCheck %s

var x : Int! = .None
if x {
  println("x is non-empty!")
}
else {
  println("an empty optional is logically false")
}
// CHECK: an empty optional is logically false

x = .Some(0)

if x {
  println("a non-empty optional is logically true")
}
else {
  println("x is empty!")
}
// CHECK: a non-empty optional is logically true

class C {}
var c : C! = C()

if c === nil {
  println("x is nil!")
} else {
  println("a non-empty class optional should not equal nil")
}
// CHECK: a non-empty class optional should not equal nil

c = nil
if c === nil {
  println("an empty class optional should equal nil")
} else {
  println("x is not nil!")
}
// CHECK: an empty class optional should equal nil
