// Testing a C++ type whose public members expose private members.
//
// The typechecker test covers most of the interesting cases, which all happen
// during semantic checking and shouldn't really affect execution. This
// executable test only exists to make sure we don't hit any unexpected
// assertions about access level invariants during latter stages of compilation,
// and does not exhaustively test runtime behavior (which is not very interesting).

// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import StdlibUnittest
import AccessInversion

var AccessInversionTestSuite = TestSuite("AccessInversion")

AccessInversionTestSuite.test("usePrivateAlias") {
    let a: Leaky.AliasToPrivateAlias = true
    expectEqual(a, true)
}

AccessInversionTestSuite.test("usePrivateRec") {
    let p = Leaky.staticReturningPrivateRec()
    // p.privateRecMethod()
    var r = Leaky.RecWithPrivateRec()
    r.mem = p
}

AccessInversionTestSuite.test("usePrivateEnum") {
    let e = Leaky.AliasToPrivateEnum(rawValue: 0)!
    expectEqual(e.rawValue, 0)
}

AccessInversionTestSuite.test("usePrivateEnumClass") {
    let e = Leaky.AliasToPrivateEnumClass(rawValue: 0)!

    switch e {
    default:
      // There is not much to test since we can't access private enums' variants
      expectEqual(e.rawValue, 0)
    }
}

AccessInversionTestSuite.test("usePrivateDefaultArgs") {
  let leaky = Leaky()
  leaky.defaultArgOfPrivateRec()
  leaky.defaultArgOfPrivateEnum()
  leaky.defaultArgOfPrivateEnumClass()
  leaky.defaultArgOfPrivateConst()
  leaky.defaultArgOfPrivateRecConst()
}

runAllTests()
