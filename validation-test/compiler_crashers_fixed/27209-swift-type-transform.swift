// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

var b{{
}
protocol a{
var d:e
}
protocol e
class b<T where a=d{
class A:a{
var d:e
