// RUN: rm -rf %t/clang-module-cache
// RUN: not --crash %swift %s -sdk %sdk -module-cache-path %t/clang-module-cache -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

extension NSSet {
if c == .b {
}
var f = 1
var e: Int -> Int = {
}
}
import Foundation
extension NSSet {
convenience init(array: Array) {
