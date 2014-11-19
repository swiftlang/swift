// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %s -sdk %sdk -module-cache-path %t/clang-module-cache -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/mattdaw (Matt Daw)

import Foundation
var a: NSMutableArray = [""]
var b: Any = a[0]
