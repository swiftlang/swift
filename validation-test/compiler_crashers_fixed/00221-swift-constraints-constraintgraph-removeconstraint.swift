// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct i<d, k: f where d.i == k>h i<i   k , d>
func f<k>(k : k) -> i
