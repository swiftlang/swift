//--- blessed.swift
// Test that all accessible inherited methods can be called.
//
// RUN: split-file %s %t
// RUN: %target-build-swift -module-name main %t/blessed.swift -I %S/Inputs -o %t/out -Xfrontend -cxx-interoperability-mode=default
// RUN: %target-codesign %t/out
// RUN: %target-run %t/out
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
        expectEqual(prot(), PROT_RETURN_VAL)
        expectEqual(priv(), PRIV_RETURN_VAL)
    }
}

extension PublBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
    }
}

extension PublPublBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
    }
}

extension ProtPublBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
    }
}

extension PrivPublBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
    }
}

extension ProtBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
    }
}


extension PublProtBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
    }
}

extension ProtProtBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
    }
}

extension PrivProtBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
    }
}

extension PrivBase {
    func ext() {
        expectEqual(publ(), PUBL_RETURN_VAL)
        expectEqual(prot(), PROT_RETURN_VAL)
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
