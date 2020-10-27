// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/FortyTwo.swiftmodule -I %S/Inputs %s

// REQUIRES: SR-13785

// TODO: Fix @_implementationOnly to consider all symbol sources

// If a symbol comes from two modules, one of which is marked as
// @_implementationOnly, Swift may choose the @_implementationOnly source
// and then error out due to the symbol being hidden.

// Swift should consider all sources for the symbol and recognize that the
// symbol is not hidden behind @_implementationOnly in all modules.

// E.g:
// In this test case, UserA and UserB both textually include `helper.h`,
// therefore both export `getFortyTwo()`.
// This test verifies that even though Swift chooses UserA.getFortyTwo(), we
// shouldn't get an error, because the symbol is also exported from UserB.

@_implementationOnly import UserA
import UserB

@_inlineable
public func callFortyTwo() -> CInt {
    return getFortyTwo()
}
