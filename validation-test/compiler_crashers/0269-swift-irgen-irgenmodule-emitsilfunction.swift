// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// https://twitter.com/AndyIbanezK/status/517759842354995200

enum A : String {
    case b = ""
}
let c: A? = nil
if c == .b {
}
