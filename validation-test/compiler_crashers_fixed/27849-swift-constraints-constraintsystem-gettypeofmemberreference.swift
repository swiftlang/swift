// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{struct a{class B:var f=B{}}enum b{struct S{struct c<T where h:a{struct S<A{struct Q{struct c,let a=c
