// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol a{
typealias e:b
typealias b{
}protocol A{typealias B:A
protocol A{typealias e:b}
}
let h:A
