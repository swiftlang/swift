// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -o %t/typedefs -Xfrontend -enable-cxx-interop -Xcc -std=c++17
// RUN: %target-codesign %t/typedefs
// RUN: %target-run %t/typedefs
//
// REQUIRES: executable_test

import Typedefs
import StdlibUnittest

var TypedefsTestSuite = TestSuite("TypedefsTestSuite")

TypedefsTestSuite.test("fully-instantiated-in-header") {
  let fruit = Avocado()
  var peeled = PeeledAvocado(fruit: fruit)
  expectEqual(peeled.peeledTaste(), 53)
}

TypedefsTestSuite.test("swift-needs-to-fully-instantiate") {
  let fruit = Banana()
  var peeled = PeeledBanana(fruit: fruit)
  expectEqual(peeled.peeledTaste(), 29)
}

TypedefsTestSuite.test("canonical-types") {
  // multiple typedeffed types with the same canonical type are the same type
  // from the typechecking perspective.
  let fruit = Banana()
  var peeled = OtherPeeledBanana(fruit: fruit)
  expectEqual(peeled.peeledTaste(), 29)

  var otherPeeled: OtherPeeledBanana = PeeledBanana(fruit: fruit)
  expectEqual(otherPeeled.peeledTaste(), 29)
}

TypedefsTestSuite.test("explicit-specialization-is-used") {
  let fruit = Cucumber()
  var peeled = PeeledCucumber(fruit: fruit)
  expectEqual(peeled.peeledTaste(), 22)
}

TypedefsTestSuite.test("using-directive-works") {
  let fruit = DragonFruit()
  var peeled = PeeledDragonFruit(fruit: fruit)
  expectEqual(peeled.peeledTaste(), 11)
}

runAllTests()
