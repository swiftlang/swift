// RUN: %target-typecheck-verify-swift -suppress-notes -I %S/Inputs -cxx-interoperability-mode=default

// In this release, the templated `operator[]` is not usable because it is given
// the wrong availability message, but should not cause a compiler crash like it
// did before. This test checks against that.

import SubscriptOverloads

var tri = TemplatedReturningInt()
tri[CInt(0)] = CInt(4) // expected-error {{'__operatorSubscript' is unavailable: use subscript}}
let _ = tri[CInt(0)] // expected-error {{'__operatorSubscript' is unavailable: use subscript}}

var tti = TemplatedTakingInt()
tti[CInt(0)] = CInt(4) // expected-error {{'__operatorSubscript' is unavailable: use subscript}}
let _: CInt = tti[CInt(0)] // expected-error {{'__operatorSubscript' is unavailable: use subscript}}
