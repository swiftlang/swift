// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

func a() -> a { // expected-error {{use of undeclared type 'a'}}
}
