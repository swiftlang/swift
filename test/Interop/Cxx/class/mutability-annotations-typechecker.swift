// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import MutabilityAnnotations

let obj = HasConstMethodAnnotatedAsMutating(a: 42) // expected-note {{change 'let' to 'var' to make it mutable}}
let i = obj.annotatedMutating() // expected-error {{cannot use mutating member on immutable value: 'obj' is a 'let' constant}}
