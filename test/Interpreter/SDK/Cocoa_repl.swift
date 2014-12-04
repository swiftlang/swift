// RUN: %swift -sdk %sdk -target %target-triple -repl < %s | FileCheck %s
// REQUIRES: sdk
// REQUIRES: swift_repl

import Cocoa

// CHECK: 0{{$}}
println(NSNumber(integer: 0).description)
