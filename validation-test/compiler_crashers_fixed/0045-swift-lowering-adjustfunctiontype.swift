// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17317691

func f() {
    ({}) // expected-error {{expression resolves to an unused function}}
}
