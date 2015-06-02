// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func f<g>() -> (g, g -> g) -> g {
  j e
}
protocol e {
protocol d : b { func b
