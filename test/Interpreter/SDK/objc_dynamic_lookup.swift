// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -i -module-cache-path=%t/clang-module-cache -sdk=%sdk %s | FileCheck %s
// REQUIRES: sdk swift_interpreter

import Foundation

// Dynamic subscripting of NSArray, dynamic method dispatch
// CHECK: 3
var array : id = [1, 2, 3, 4, 5]
println(array[2]!.description!())

// Dynamic subscripting of NSDictionary, dynamic method dispatch
// CHECK: 2
var dict : NSDictionary = ["Hello" : 1, "World" : 2]
println(dict["World"]!.description!())
