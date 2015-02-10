// RUN: %target-repl-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_repl

import Cocoa

// CHECK: 0{{$}}
println(NSNumber(integer: 0).description)
