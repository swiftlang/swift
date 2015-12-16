// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

enum S<T where g:a{enum A{class c{let t}struct a{let a=c()}}struct S{var _=A
