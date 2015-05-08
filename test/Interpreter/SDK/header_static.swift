// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import MapKit

let rect = MKMapRectMake(1.0, 2.0, 3.0, 4.0)
// CHECK: {{^}}1.0 2.0 3.0 4.0{{$}}
print("\(rect.origin.x) \(rect.origin.y) \(rect.size.width) \(rect.size.height)")

let value: CUnsignedInt = 0xFF00FF00
// CHECK: {{^}}ff00ff00 ff00ff{{$}}
print("\(String(value, radix: 16)) \(String(NSSwapInt(value), radix: 16))")
