// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class B<T where g:a{class a<T where g:T{{}let f=a<T}class a{func f<var f=a{
