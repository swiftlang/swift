// RUN: %target-swift-frontend %s -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// https://devforums.apple.com/message/1051862

func a() {
    var b = [[Int]]()
    for c in 0..<1 {
        b[c][c] = 1
    }
}
