// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift_driver -emit-module -sdk %sdk -o %t/test.swiftmodule %s
// RUN: %swift_driver -g -module-cache-path %t/clang-module-cache -sdk %sdk -o %t/sdk-link %s
// RUN: %t/sdk-link | FileCheck %s
// REQUIRES: sdk

import Foundation

// CHECK: {{^}}ABCDEF{{$}}
println(("ABC" as NSString).stringByAppendingString("DEF"))
