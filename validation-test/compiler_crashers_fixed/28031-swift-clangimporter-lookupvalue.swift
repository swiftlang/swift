// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class b{var _=B<b}{class B<T where T:A{class A<T{enum A{class a{enum b{var _=B<T struct B class B
