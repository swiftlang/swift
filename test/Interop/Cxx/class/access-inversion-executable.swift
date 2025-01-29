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
    // TODO: should privateRecMethod() be accessible, even if we can't name its type?
    // p.privateRecMethod()
    var r = Leaky.RecWithPrivateRec()
    r.mem = p
}

AccessInversionTestSuite.test("usePrivateEnum") {
    let e = Leaky.AliasToPrivateEnum(rawValue: 0)!
    expectEqual(e.rawValue, 0)
}

AccessInversionTestSuite.test("usePrivateEnumClass") {
    // TODO: should private enum class members be accessible?
    // let e = Leaky.AliasToPrivateEnumClass.privateEnumClassMember

    let e = Leaky.AliasToPrivateEnumClass(rawValue: 0)!

    switch e {
    // TODO: should private enum class members be accessible?
    // case .privateEnumClassMember:
    default:
      // There is not much to test since we can't actually access variants
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
