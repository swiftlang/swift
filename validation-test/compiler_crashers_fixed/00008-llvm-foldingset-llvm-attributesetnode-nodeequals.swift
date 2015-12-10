// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

func ^(a: BooleanType, Bool) -> Bool {
    return !(a)
}
