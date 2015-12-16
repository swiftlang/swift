// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{
class A c{class A:A:class B<T{{{{{}}}}class b:A
}}class B<T where h:C{class b
let f=b(
