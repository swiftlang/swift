// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

let x = NSObject()
unowned let y = x

println(y) // CHECK:      <NSObject: [[ID:.*]]>
println(x) // CHECK-NEXT: <NSObject: [[ID]]>
