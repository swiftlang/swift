// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

var x : @unchecked Int? = .None
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
