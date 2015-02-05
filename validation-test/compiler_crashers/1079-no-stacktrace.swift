// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

extension NSSet {
func g<T where T.E == F>(f: B<T>) {
}
}
func b(c) -> <d>(() -> d) {
}
import Foundation
extension NSSet {
convenience init(array: Array) {
