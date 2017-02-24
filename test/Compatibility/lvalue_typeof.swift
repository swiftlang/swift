// RUN: %target-typecheck-verify-swift -swift-version 4

// Make sure an r-value coercion is performed on the argument in Swift 4 mode.
var x: Int = 1
_ = type(of: x)

