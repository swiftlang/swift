// RUN: rm -rf %t/clang-module-cache
// RUN: not %target-swift-frontend %s -module-cache-path %t/clang-module-cache -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

import Foundation
class m<j>k i<g : g, e : f k(f: l) {
}
i(())
class h {
    typealias g = g
