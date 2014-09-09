// RUN: not --crash %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

// XFAIL: asan

protocol a : a {
}
