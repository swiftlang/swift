// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

protocol a : a { // expected-error{{circular protocol inheritance a}}
}
