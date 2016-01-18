// RUN: not %target-swift-frontend %s -parse
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct S<T where a:B{struct Q<T{enum a{class b{{}struct B<T where H:A}}struct B:A{}protocol A
