// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A{{}{}enum B<T where T:d}struct Q{struct B{enum A{class A{struct S<T where B:A
