// RUN: not --crash %swift %s -emit-ir
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

// XFAIL: asan

class A<T : A> {
}
