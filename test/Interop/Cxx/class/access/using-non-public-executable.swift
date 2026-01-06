// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers)
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

import StdlibUnittest
import UsingNonPublic

var Suite = TestSuite("Using Base methods")

Suite.test("PublUser: using statements with public inheritance") {
  let p = PublUser()
  expectEqual(p.publUsingPubl(), Return.publUsingPubl)
  expectEqual(p.publUsingProt(), Return.publUsingProt)
  expectEqual(p.omitUsingPubl(), Return.omitUsingPubl)
}

Suite.test("ProtUser: using statements with protected inheritance") {
  let p = ProtUser()
  expectEqual(p.publUsingPubl(), Return.publUsingPubl)
  expectEqual(p.publUsingProt(), Return.publUsingProt)
}

Suite.test("PrivUser: using statements with private inheritance") {
  let p = PrivUser()
  expectEqual(p.publUsingPubl(), Return.publUsingPubl)
  expectEqual(p.publUsingProt(), Return.publUsingProt)
}

Suite.test("PublPrivUser: inheriting using statements") {
  let p = PublPrivUser()
  expectEqual(p.publUsingPubl(), Return.publUsingPubl)
  expectEqual(p.publUsingProt(), Return.publUsingProt)
}

Suite.test("PrivUserPubl: using inherited members") {
  let p = PrivUserPubl()
  expectEqual(p.publUsingPubl(), Return.publUsingPubl)
  expectEqual(p.publUsingProt(), Return.publUsingProt)
}

runAllTests()
