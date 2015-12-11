// RUN: %target-swift-frontend %s -parse -verify

// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17986597
// Similar to 010-circular-protocol-reference.swift, might be same underlying bug.

protocol A {
    typealias A = B
}

protocol B {
    typealias B = A
}
