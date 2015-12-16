// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class a<T where a:a{class a{struct d{class B:a{protocol C{class b{class B<where T{}}
