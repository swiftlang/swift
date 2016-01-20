// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case found by https://github.com/robrix (Rob Rix)
// http://www.openradar.me/19924870

func unit<T>(x: T) -> T? {
    return x
}
func f() -> Int? {
    return unit(1) ?? unit(2).map { 1 } ?? nil
}
