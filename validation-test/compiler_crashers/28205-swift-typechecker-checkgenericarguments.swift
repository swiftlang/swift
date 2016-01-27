// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{struct X<a{protocol A{class B<T>:B<T>let h:A