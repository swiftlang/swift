// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17157251

// XFAIL: asan

// Case 1.
class A: A { // expected-error {{circular class inheritance A}}
}

// Case 2.
class B : C { // expected-error {{circular class inheritance B}}
}

typealias C = B
