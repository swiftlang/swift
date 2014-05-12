// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-build-swift -emit-module -o %t/test.swiftmodule %s
// RUN: %target-build-swift -g -module-cache-path %t/clang-module-cache -o %t/sdk-link %s
// RUN: %target-run %t/sdk-link | FileCheck %s
// REQUIRES: sdk

import Foundation

// CHECK: {{^}}ABCDEF{{$}}
println(("ABC" as NSString).stringByAppendingString("DEF"))
