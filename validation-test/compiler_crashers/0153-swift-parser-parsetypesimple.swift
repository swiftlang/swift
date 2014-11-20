// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func i(f: g) -> <j>(() -> j) -> g { func g
k, l {
    typealias l = m<k<m>, f>
}
