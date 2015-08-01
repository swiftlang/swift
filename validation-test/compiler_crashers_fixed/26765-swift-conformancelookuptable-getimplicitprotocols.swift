// RUN: not %target-swift-frontend %s -parse
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class B<T where B:B{struct B<T where g:b{let e=B}struct B
