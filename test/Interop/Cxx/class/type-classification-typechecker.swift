// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import TypeClassification

let x = HasMethodThatReturnsTemplatedBox()
let box = x.getIntBox()
let unsafeBox = x.__getIntBoxUnsafe() // expected-error {{value of type 'HasMethodThatReturnsTemplatedBox' has no member '__getIntBoxUnsafe'}}
