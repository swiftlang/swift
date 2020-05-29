// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -o %t/typedefs -Xfrontend -enable-cxx-interop -Xcc -std=c++17
// RUN: %target-codesign %t/typedefs
// RUN: %target-run %t/typedefs
//
// REQUIRES: executable_test

import Typedefs
import StdlibUnittest

var TypedefsTestSuite = TestSuite("TypedefsTestSuite")

TypedefsTestSuite.test("foo") {
  let banana = Banana()
  var peeled: PeeledBanana = PeeledBanana(t: banana)
  expectEqual(peeled.doPeel(), 43)
}

TypedefsTestSuite.test("canonical-types") {
  // multiple typedeffed types with the same canonical type are the same type
  // from the typechecking perspective.
  let banana: Banana = Banana()
  var peeled: PeeledBanana = OtherPeeledBanana(t: banana)
  expectEqual(peeled.doPeel(), 43)

  var otherPeeled: OtherPeeledBanana = PeeledBanana(t: banana)
  expectEqual(otherPeeled.doPeel(), 43)
}

runAllTests()
