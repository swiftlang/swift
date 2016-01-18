// RUN: not %target-swift-frontend %s -parse
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

enum S<T where g:e{{}struct a{
struct B<t>:S
struct S<T where g:t
func b{
B
