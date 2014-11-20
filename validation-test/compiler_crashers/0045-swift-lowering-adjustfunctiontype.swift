// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17317691

func f() {
    ({}) // expected-error {{type of expression is ambiguous without more context}}
}
