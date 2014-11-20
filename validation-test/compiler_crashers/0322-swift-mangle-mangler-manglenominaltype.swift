// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18637242

protocol P {
    init(x: Int? = nil) // expected-error {{default argument not permitted in a protocol initializer}}
}
