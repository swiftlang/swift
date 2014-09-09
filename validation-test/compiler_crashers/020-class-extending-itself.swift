// RUN: not --crash %swift %s -emit-ir
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17157251

// XFAIL: asan

// Case 1.
class A: A {
}

// Case 2.
class B : C {
}

typealias C = B
