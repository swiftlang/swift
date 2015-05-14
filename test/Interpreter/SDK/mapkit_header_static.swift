// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop
// UNSUPPORTED: OS=tvos

import MapKit

let rect = MKMapRectMake(1.0, 2.0, 3.0, 4.0)
// CHECK: {{^}}1.0 2.0 3.0 4.0{{$}}
print("\(rect.origin.x) \(rect.origin.y) \(rect.size.width) \(rect.size.height)")
