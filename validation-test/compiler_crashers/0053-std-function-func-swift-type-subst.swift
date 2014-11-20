// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/julasamer (julasamer)

struct c<d, e: b where d.c == e> { // expected-error {{use of undeclared type 'b'}} expected-error {{'c' is not a member type of 'd'}}
}
