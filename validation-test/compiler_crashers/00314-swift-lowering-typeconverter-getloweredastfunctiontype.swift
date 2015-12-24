// RUN: not --crash %target-swift-frontend %s -emit-silgen

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

enum e<b> : d {
    func c<b>() -> b {
     }
}
protocol d {
    func c<b>()(_: b) -> b
}
