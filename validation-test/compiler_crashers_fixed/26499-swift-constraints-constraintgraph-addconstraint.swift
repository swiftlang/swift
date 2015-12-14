// RUN: not %target-swift-frontend %s -parse
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func a<struct c class d<T where T:a{
let f=a<c
struct A
{
class B<T
let d=B<T
