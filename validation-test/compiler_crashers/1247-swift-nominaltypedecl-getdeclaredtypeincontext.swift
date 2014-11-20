// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

var e: Int -> Int = {
return $0
struct d<f : e, g: e where g.h = f.h> { : C {
func g<T where T>(f: B<T>) {
}
}
class A {
