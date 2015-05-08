// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

let x = NSObject()
unowned let y = x

print(y) // CHECK:      <NSObject: [[ID:.*]]>
print(x) // CHECK-NEXT: <NSObject: [[ID]]>
