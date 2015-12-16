// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/austinzheng (Austin Zheng)

enum A<T> {
    case Just(T)
    case Error
}

func foo() -> A<(String, String?)> {
    return A.Just("abc", "def")
}
