// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct d<T where g:t{protocol a{func b<d where g.E=C{}}class C{class A{class B<T>:B<T>
