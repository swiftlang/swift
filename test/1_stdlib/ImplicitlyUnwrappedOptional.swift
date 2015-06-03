// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

var x : Int! = .None
if x != nil {
  print("x is non-empty!")
}
else {
  print("an empty optional is logically false")
}
// CHECK: an empty optional is logically false

x = .Some(0)

if x != nil {
  print("a non-empty optional is logically true")
}
else {
  print("x is empty!")
}
// CHECK: a non-empty optional is logically true

class C {}
var c : C! = C()

if c === nil {
  print("x is nil!")
} else {
  print("a non-empty class optional should not equal nil")
}
// CHECK: a non-empty class optional should not equal nil

c = nil
if c === nil {
  print("an empty class optional should equal nil")
} else {
  print("x is not nil!")
}
// CHECK: an empty class optional should equal nil

import StdlibUnittest
import Swift

var ImplicitlyUnwrappedOptionalTests = TestSuite("ImplicitlyUnwrappedOptional")

ImplicitlyUnwrappedOptionalTests.test("flatMap") {
  // FIXME(19798684): can't call map or flatMap on ImplicitlyUnwrappedOptional

  // let half: Int32 -> Int16! =
  //   { if $0 % 2 == 0 { return Int16($0 / 2) } else { return .None } }

  // expectOptionalEqual(2 as Int16, half(4))
  // expectEmpty(half(3))

  // expectEmpty((.None as Int!).flatMap(half))
  // expectOptionalEqual(2 as Int16, (4 as Int!).flatMap(half))
  // expectEmpty((3 as Int!).flatMap(half))
}

runAllTests()

