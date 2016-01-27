// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

// Crash type: memory error ("Invalid read of size 4")
var a{class d{var b=B{}let c=(x:d<T{{}}class B<T where h=d>:a
