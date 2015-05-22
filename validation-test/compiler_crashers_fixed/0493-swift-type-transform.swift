// RUN: not %target-swift-frontend %s -parse


// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class d<c>: NSObject {
<d>(() -> d) {
}
enum S<T> : P {
func f<T>() -> T -> T {
}
protocol P {
func f>()(
