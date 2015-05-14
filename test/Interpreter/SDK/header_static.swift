// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

let value: CUnsignedInt = 0xFF00FF00
// CHECK: {{^}}ff00ff00 ff00ff{{$}}
print("\(String(value, radix: 16)) \(String(NSSwapInt(value), radix: 16))")
