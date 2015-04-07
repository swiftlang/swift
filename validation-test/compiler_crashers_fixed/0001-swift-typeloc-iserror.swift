// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17157251

// Case #1
protocol a : a { // expected-error {{circular protocol inheritance a}}
}
// Case #2 (appears to trigger same bug)
class A : A { // expected-error {{circular class inheritance A}}
}
class B : C { // expected-error {{circular class inheritance B}}
}
typealias C = B
