// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

func ^(a: BooleanType, Bool) -> Bool {
    return !(a) // expected-error {{cannot invoke '!' with an argument of type 'BooleanType'}}
}
