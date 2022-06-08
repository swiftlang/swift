// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

// REQUIRES: rdar91961524

import MutableMembers

let obj = HasPublicMutableMember(a: 42) // expected-note {{change 'let' to 'var' to make it mutable}}
let i = obj.foo() // expected-error {{cannot use mutating member on immutable value: 'obj' is a 'let' constant}}
