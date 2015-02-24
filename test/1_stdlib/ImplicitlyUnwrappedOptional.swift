// RUN: %target-run-simple-swift | FileCheck %s

// XFAIL: linux

var x : Int! = .None
if x != nil {
  println("x is non-empty!")
}
else {
  println("an empty optional is logically false")
}
// CHECK: an empty optional is logically false

x = .Some(0)

if x != nil {
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

import StdlibUnittest
import Swift

var ImplicitlyUnwrappedOptionalTests = TestSuite("ImplicitlyUnwrappedOptional")

let half : Int -> Int! =
  { if $0 % 2 == 0 { return $0 / 2 } else { return .None } }

ImplicitlyUnwrappedOptionalTests.test("flatMap") {
  // FIXME(19798684): can't call map or flatMap on ImplicitlyUnwrappedOptional
  // expectTrue(ImplicitlyUnwrappedOptional<Int>.None.flatMap(half) ==
  //   ImplicitlyUnwrappedOptional<Int>.None)
  // expectTrue(half(4) == ImplicitlyUnwrappedOptional<Int>.Some(2))
  // expectTrue(half(4).flatMap(half) == ImplicitlyUnwrappedOptional<Int>.Some(1))
  // expectTrue(half(4).flatMap(half).flatMap(half) ==
  //   ImplicitlyUnwrappedOptional<Int>.None)
}

runAllTests()

