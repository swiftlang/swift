// RUN: %target-repl-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_repl

import Cocoa

// CHECK: 0{{$}}
print(NSNumber(integer: 0).description)
