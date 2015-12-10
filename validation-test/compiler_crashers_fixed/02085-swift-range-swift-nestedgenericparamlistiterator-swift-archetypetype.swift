// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func a
extension NSSet {
}
func a: CollectionType where T -> ()
protocol P {
func b<T : b) -> {
