// RUN: %target-parse-verify-swift

Array.map // expected-error{{argument for generic parameter 'T' could not be inferred}}

let a = [1, 2, 3]
a.map // expected-error{{argument for generic parameter 'U' could not be inferred}}

