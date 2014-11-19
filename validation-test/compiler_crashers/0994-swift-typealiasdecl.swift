// RUN: rm -rf %t/clang-module-cache
// RUN: not --crash %swift %s -sdk %sdk -module-cache-path %t/clang-module-cache -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

import Foundation
class d<c>: NSObject {
var b: c
func f<g>() -> (g, g -> g) -> g {
e e: ((g, g -> g) -> g)!
}
protocol e {
protocol d : b { func b
