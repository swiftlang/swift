// RUN: rm -rf %t/clang-module-cache
// RUN: not %target-swift-frontend %s -module-cache-path %t/clang-module-cache -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

import Foundation
f = compose(b> : e: String = "A? = A(g<(b: C {
class func g<T) {
struct c = 0
var f = c()
