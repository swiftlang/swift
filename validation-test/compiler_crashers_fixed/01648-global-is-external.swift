// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18850296

func f() {
    let x = 0 // expected-note {{'x' declared here}} //expected-warning {{initialization of immutable value 'x' was never used}}
    class C { // expected-note {{type declared here}}
        func f() {
            x // expected-error {{class declaration cannot close over value 'x' defined in outer scope}}
        }
    }
}
