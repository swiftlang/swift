// Test that all accessible inherited methods can be called.
//
// Note that this test isn't very meaningful until we are allowed to access
// non-public members (those are the TODOs). It is checked in for now as
// a placeholder (and it _does_ at least ensure that things haven't gone
// horribly wrong).
//
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import StdlibUnittest
import NonPublicInheritance

var Tests = TestSuite("NonPublicInheritance")
Tests.test("Base") { Base().ext() }
Tests.test("PublBase") { PublBase().ext() }
Tests.test("ProtBase") { ProtBase().ext() }
Tests.test("PrivBase") { PrivBase().ext() }
Tests.test("PublPublBase") { PublPublBase().ext() }
Tests.test("ProtPublBase") { ProtPublBase().ext() }
Tests.test("PrivPublBase") { PrivPublBase().ext() }
Tests.test("PublProtBase") { PublProtBase().ext() }
Tests.test("ProtProtBase") { ProtProtBase().ext() }
Tests.test("PrivProtBase") { PrivProtBase().ext() }
Tests.test("PublPrivBase") { PublPrivBase().ext() }
Tests.test("ProtPrivBase") { ProtPrivBase().ext() }
Tests.test("PrivPrivBase") { PrivPrivBase().ext() }

extension Base {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        // TODO: prot()
        // TODO: priv()
    }
}

extension PublBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        // TODO: prot()
    }
}

extension PublPublBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        // TODO: prot()
    }
}

extension ProtPublBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
    }
}

extension PrivPublBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
    }
}

extension ProtBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
    }
}


extension PublProtBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
    }
}

extension ProtProtBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
    }
}

extension PrivProtBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
    }
}

extension PrivBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
    }
}

extension PublPrivBase {
    func ext() {
      // Nothing to test (nothing is accessible)
    }
}

extension ProtPrivBase {
    func ext() {
      // Nothing to test (nothing is accessible)
    }
}

extension PrivPrivBase {
    func ext() {
      // Nothing to test (nothing is accessible)
    }
}

runAllTests()
