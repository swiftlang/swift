// RUN: not %target-swift-frontend %s -parse
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol a{struct B<I:A}class A{{}protocol A{func b:A
