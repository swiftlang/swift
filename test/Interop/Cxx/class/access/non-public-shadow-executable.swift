// Check that we are resolving the correct member for all unambiguous members
// in the Shadow struct.

// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers)
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

import StdlibUnittest
import NonPublicShadow

var Tests = TestSuite("NonPublicShadow")

Tests.test("Check return values") {
  var s = Shadow()

  expectEqual(s.publOrPriv(), Return.Publ_publOrPriv)

  expectEqual(s.publPublShadowed(), Return.Shadow_publPublShadowed)
  expectEqual(s.protPublShadowed(), Return.Shadow_protPublShadowed)
  expectEqual(s.privPublShadowed(), Return.Shadow_privPublShadowed)
}

runAllTests()
