// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/jvasileff (John Vasileff)
// This bug is NOT triggered when compiling with -O.

func f<T : BooleanType>(b: T) { // expected-note {{in call to function 'f'}}
}
f(true as BooleanType) // expected-error {{generic parameter 'T' is constrained to a non-@objc protocol type 'BooleanType' and cannot be bound to an existential type}}
