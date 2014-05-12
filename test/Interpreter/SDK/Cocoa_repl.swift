// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -sdk %sdk -repl < %s | FileCheck %s
// REQUIRES: sdk
// REQUIRES: swift_repl

import Cocoa

// CHECK: 0{{$}}
println(NSNumber(integer: 0).description)
