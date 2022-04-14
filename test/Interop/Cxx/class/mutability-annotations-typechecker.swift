// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop \
// RUN:     -verify -verify-additional-file %S/Inputs/mutability-annotations.h -verify-ignore-unknown

import MutabilityAnnotations

let obj = HasConstMethodAnnotatedAsMutating(a: 42) // expected-note {{change 'let' to 'var' to make it mutable}}
let i = obj.annotatedMutating() // expected-error {{cannot use mutating member on immutable value: 'obj' is a 'let' constant}}

let objWMutableProperty = HasMutableProperty(a: 42, b: 21) // expected-note {{change 'let' to 'var' to make it mutable}}
// expected-note@-1 {{change 'let' to 'var' to make it mutable}}

let _ = objWMutableProperty.annotatedNonMutating()
let _ = objWMutableProperty.noAnnotation() // expected-error {{cannot use mutating member on immutable value: 'objWMutableProperty' is a 'let' constant}}
let _ = objWMutableProperty.contradictingAnnotations() // expected-error {{cannot use mutating member on immutable value: 'objWMutableProperty' is a 'let' constant}}
let _ = objWMutableProperty.duplicateAnnotations()
