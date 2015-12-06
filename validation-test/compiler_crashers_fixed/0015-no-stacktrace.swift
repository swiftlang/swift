// RUN: not %target-swift-frontend %s -emit-silgen

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17225563

enum a<T> {
    case s(T, a)
}
