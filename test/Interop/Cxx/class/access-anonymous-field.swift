// RUN: %target-typecheck-verify-swift -I %S/Inputs -swift-version 6 -cxx-interoperability-mode=upcoming-swift

// CHECK: Foobar

import SimpleStructs

let s = HasAnonymousType(1, 2, 3)
let _ = s.__Anonymous_field0 // expected-error {{'__Anonymous_field0' is unavailable: refer to the members of the anonymous type instead}}
// Referring to the members of the anonymous type directly.
let _ = s.a
let _ = s.b
let _ = s.c
