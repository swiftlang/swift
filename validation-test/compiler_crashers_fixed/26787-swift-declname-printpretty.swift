// RUN: not %target-swift-frontend %s -parse
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func a{struct B<T where g:a{struct S<h{class A{struct B<struct B<T where g:a{var:A
