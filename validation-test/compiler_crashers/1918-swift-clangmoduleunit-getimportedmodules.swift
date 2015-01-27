// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

import Foundation
func g<b: Int = F>(c, object2: (a: Any) in 0)
protocol B : b {
typealias b
