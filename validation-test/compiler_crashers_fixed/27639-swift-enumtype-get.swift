// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{
enum S<T{
enum A
enum S<T where B:C{enum S<T where T:T>:A
