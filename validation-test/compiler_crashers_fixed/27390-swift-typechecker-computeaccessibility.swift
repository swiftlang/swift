// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{struct B{struct Q{struct T{class a{struct B{struct A{struct A<T where g:C{struct A{var d{A{
