// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default -enable-upcoming-feature MemberImportVisibility)
//
// REQUIRES: executable_test
// REQUIRES: swift_feature_MemberImportVisibility

import InheritedLookup
import StdlibUnittest

var InheritedMemberTestSuite = TestSuite("Test if inherited lookup works")

extension Bar.Derived {
    public func callBase() {
        let _ = baseMethod() 
    }
}

InheritedMemberTestSuite.test("Look up base methods from extensions") {
  let a = Bar.Derived()
  a.callBase()
}

runAllTests()
